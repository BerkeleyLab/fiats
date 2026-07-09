! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

program infer_aerosol

  ! External dependencies:
  use fiats_m, only : unmapped_network_t, tensor_t, double_precision, double_precision_file_t
  use julienne_m, only : &
     call_julienne_assert_ &
    ,command_line_t &
    ,csv &
    ,string_t &
    ,operator(.approximates.) &
    ,operator(.all.) &
    ,operator(.equalsExpected.) &
    ,operator(.within.)
  use adon_arrays_m, only : adon_arrays_t
  use omp_lib
  use iso_fortran_env, only : int64, real64

  ! Internal dependencies:
  use NetCDF_file_m, only: NetCDF_file_t
  implicit none

  type tensor_statistics_t
    double precision, allocatable, dimension(:) :: mean, standard_deviation
  end type

  character(len=*), parameter :: usage_info = & 
    new_line('') // &
    'Usage:  ./build/run-fpm.sh run infer-aerosol --  --file-path <string>' // &
    'where angular brackets (<>) denote user-provided input.' // &
    new_line('')

  call read_stats_and_perform_inference( file_path( stop_code = usage_info ) )

contains

  function file_path(stop_code) result(path)
    character(len=:), allocatable :: path
    character(len=*), intent(in) :: stop_code
    type(command_line_t) command_line

    path = command_line%flag_value("--file-path") 
    if (len(path)==0) error stop stop_code
    path = path // "/"
  end function

  subroutine read_stats_and_perform_inference(path)
    character(len=*), intent(in) :: path
    integer,          parameter :: branch_net_inputs = 39, branch_net_outputs = 20
    integer,          parameter :: trunk_net_inputs = 4, trunk_net_outputs = 20
    character(len=*), parameter :: branch_file_name = "model_branch.json", trunk_file_name = "model_trunk.json"
    character(len=*), parameter :: saved_data_file_name = "saved_data2.nc"
    double precision, allocatable, dimension(:) :: longitude, latitude, level, ymean, mean_X, std_X, mean_y, std_y
    double precision, allocatable, dimension(:,:) :: X_test, branch_inputs, trunk_inputs
    double precision, allocatable, dimension(:,:), target :: basis
    double precision, allocatable, dimension(:,:) :: cldfr_idx, slice
    type(unmapped_network_t(double_precision)) branch_network, trunk_network
    integer(selected_int_kind(18)) t_start, t_finish, clock_rate
    type(tensor_t(double_precision)), allocatable :: outputs(:)
    integer i, j

    associate(saved_data_file => NetCDF_file_t(path // saved_data_file_name))
      print '(a)', "Reading saved data from " // path // saved_data_file_name
      call saved_data_file%input("longitude", longitude)
      call saved_data_file%input("latitude", latitude)
      call saved_data_file%input("level", level)
      call saved_data_file%input("cldfr_idx", cldfr_idx)
      call saved_data_file%input("basis", basis)
      call saved_data_file%input("ymean", ymean)
      call saved_data_file%input("X_test", X_test)
      call saved_data_file%input("mean_X", mean_X)
      call saved_data_file%input("std_X", std_X)
      call saved_data_file%input("mean_y", mean_y)
      call saved_data_file%input("std_y", std_y)
    end associate

    slice = testslice(cldfr_idx, level, longitude, latitude, 72)

    print *, "Reading the branch network from " // branch_file_name
    branch_network = unmapped_network_t(double_precision_file_t(path // branch_file_name))
    print *, "Reading the trunk network from " // trunk_file_name
    trunk_network = unmapped_network_t(double_precision_file_t(path // trunk_file_name))

    allocate(branch_inputs(size(X_test,1),size(X_test,2)))

    do concurrent(i = 1:size(X_test,1),  j = 1:size(X_test,2))  default(none) shared(X_test, branch_inputs, mean_X, std_X)
      associate(cube_root => (abs(X_test(i,j))**(1.d0/3.d0))*sign(1.d0,X_test(i,j)))
        branch_inputs(i,j) = (cube_root - mean_X(i))/std_X(i)
      end associate
    end do

    time_inference: &
    block
      integer(int64) t_start_dc, t_end_dc, clock_rate, t_start_omp, t_end_omp
      type(tensor_t(double_precision)), allocatable :: branch_outputs(:), trunk_outputs(:)
      real, allocatable :: output_slice(:), out(:,:)
      integer s

      count_samples: &
      associate(samples => size(branch_inputs,2))

        allocate(branch_outputs(samples))

        print '(2(a,i))', "Starting branch inference via `do concurrent` with ", samples, " samples of size ",size(branch_inputs,1)
        call system_clock(t_start_dc, clock_rate)
        do concurrent(integer :: s = 1:samples) default(none) shared(branch_outputs, branch_network, branch_inputs)
          branch_outputs(s) = branch_network%infer(tensor_t(branch_inputs(:,s)))
        end do
        call system_clock(t_end_dc)
        print *,"Elapsed system clock during `do concurrent`: ", real(t_end_dc - t_start_dc, real64)/real(clock_rate, real64)

        allocate(trunk_inputs, mold=slice)

        associate(ncol => size(mean_X))
          do concurrent(i = 1:size(slice,1),  j = 1:size(slice,2)) ! default(none) shared(slice,input_components)
            trunk_inputs(i,j) = (slice(i,j) - mean_X(ncol-4+i))/std_X(ncol-4+i)
          end do
        end associate

        allocate(trunk_outputs(samples))

        print*, "Starting trunk inference via `do concurrent` with ", samples, " samples of size ",size(trunk_inputs,1)
        call system_clock(t_start_dc, clock_rate)
        do concurrent(integer :: s = 1:samples) default(none) shared(trunk_outputs, trunk_network, trunk_inputs)
          trunk_outputs(s) = trunk_network%infer(tensor_t(trunk_inputs(:,s)))
        end do
        call system_clock(t_end_dc)
        print *,"Elapsed system clock during `do concurrent`: ", real(t_end_dc - t_start_dc, real64)/real(clock_rate, real64)

        call_julienne_assert(.all. (shape(basis) .equalsExpected. [20,20]))

        print*, "Starting concatenation" 
        block
          double precision, allocatable, dimension(:,:,:) :: aug_trunk, basis_repeated
          double precision, allocatable, dimension(:,:)   :: raw_trunk_outputs, raw_branch_outputs, branch_dot_trunk, final_output

          associate(m=> size(trunk_outputs), n => size(trunk_outputs(1)%values()))

            print *,"Allocating basis_repeated to shape ", size(basis,1), size(basis,2), m

            ! Copy basis (shape [20,20]) m times to form basis_repeated (shape [20,20,m])

            allocate(basis_repeated(size(basis,1), size(basis,2), m))

            do concurrent(integer :: b = 1:m) default(none) shared(basis, basis_repeated)
              basis_repeated(:,:,b) = basis
            end do

            print *,"Allocating raw_trunk_outputs to shape ", n, m

            allocate(raw_trunk_outputs(n,m))

            do concurrent(integer :: s=1:size(trunk_outputs)) default(none) shared(raw_trunk_outputs, trunk_outputs)
              raw_trunk_outputs(:,s) = trunk_outputs(s)%values()
            end do

            call_julienne_assert(.all. (shape(raw_trunk_outputs) .equalsExpected. [20,m]))
            call_julienne_assert(.all. (shape(basis_repeated) .equalsExpected. [20,20,m]))

            ! Concatenate basis_repeated (shape [20,20,m]) with raw_trunk_outputs (shape [20,m])
            ! to form aug_trunk (shape [n+1,n,m])
            allocate(aug_trunk(n+1, n, m))
            aug_trunk(1:n, :, :) = basis_repeated
            aug_trunk(n+1, :, :) = raw_trunk_outputs

            adon_check: &
            associate(adon_arrays => adon_arrays_t(path="adon"))

               print *,"shape(aug_trunk_outputs) =", shape(aug_trunk)
               print *,"shape(adon_arrays%trunk_output) =", shape(adon_arrays%trunk_output)

               call_julienne_assert(.all. (shape(aug_trunk(:,:,1:10)) .equalsExpected.  shape(adon_arrays%trunk_output)))
               call_julienne_assert(.all. (aug_trunk(:,:,1) .approximates.  adon_arrays%trunk_output(:,:,1) .within. 1D-03))

            print *,"Allocating raw_branch_outputs to shape ", size(branch_outputs(1)%values()), size(branch_outputs)
            allocate(raw_branch_outputs(size(branch_outputs(1)%values()),size(branch_outputs)))

            call_julienne_assert(.all. (shape(raw_branch_outputs) .equalsExpected. [21,m]))

            do concurrent(integer :: s=1:size(branch_outputs)) default(none) shared(raw_branch_outputs, branch_outputs)
              raw_branch_outputs(:,s) = branch_outputs(s)%values()
            end do

            print *,"Allocating branch_dot_trunk to shape ", size(aug_trunk,2), size(aug_trunk,3)

            allocate(branch_dot_trunk(size(aug_trunk,2), size(aug_trunk,3)))

            ! Inner product (n,B): (21,) * (21,20) -> (20,) for each sample b
            do concurrent(integer :: b = 1:size(aug_trunk,3)) default(none) shared(branch_dot_trunk, aug_trunk, raw_branch_outputs)
               branch_dot_trunk(:,b) = matmul(raw_branch_outputs(:,b), aug_trunk(:,:,b))
            end do

            call_julienne_assert(.all.(shape(branch_dot_trunk(:,1:10)) .equalsExpected. shape(adon_arrays%branch_dot_trunk)))

            call_julienne_assert(.all.(adon_arrays%branch_dot_trunk .approximates. branch_dot_trunk(:,1:10) .within. 1D-03))

            end associate adon_check

            print *, "adon_check complete"

            allocate(final_output(size(branch_dot_trunk,1), size(branch_dot_trunk,2)))

            call_julienne_assert(size(final_output,1) .equalsExpected. size(ymean))
            call_julienne_assert(size(final_output,1) .equalsExpected. size(mean_y))
            do concurrent(integer :: i = 1:size(final_output,1)) default(none) shared(branch_dot_trunk, final_output, ymean, mean_y, std_y)
               final_output(i,:) = ((branch_dot_trunk(i,:) + ymean(i) + mean_y(i)) * std_y(i))**3
            end do

            ! do concurrent(i = 1:size(X_test,1),  j = 1:size(X_test,2))  default(none) shared(X_test, branch_inputs, mean_X, std_X)
            !    associate(cube_root => (abs(X_test(i,j))**(1.d0/3.d0))*sign(1.d0,X_test(i,j)))
            !      branch_inputs(j,i) = (cube_root - mean_X(i))/std_X(i)
            !    end associate
            ! end do
          end associate
        end block
    end associate count_samples

    end block time_inference

  end subroutine read_stats_and_perform_inference

  function read_tensor_statistics(mean_file, standard_deviation_file, n) result(tensor_statistics)
    type(tensor_statistics_t) tensor_statistics
    character(len=*), intent(in) :: mean_file, standard_deviation_file
    integer, intent(in) :: n
    integer i, mean_unit, standard_deviation_unit
  
    open(newunit = mean_unit, file = mean_file, status="old")
    open(newunit = standard_deviation_unit , file = standard_deviation_file, status="old")

    allocate(tensor_statistics%mean(n))
    allocate(tensor_statistics%standard_deviation(n))
  
    do i = 1, n
      read(mean_unit, *) tensor_statistics%mean(i)
      read(standard_deviation_unit , *) tensor_statistics%standard_deviation(i)
    end do

    close(mean_unit)
    close(standard_deviation_unit)
  end function

  function testslice(cldfr_idx, level, longitude, latitude, i) result(X_test_aug)
    double precision, intent(in) :: level(:), longitude(:), latitude(:)
    double precision, intent(in) :: cldfr_idx(:,:)
    integer, intent(in) :: i
    double precision :: X_test_aug(4, size(cldfr_idx,1))

    X_test_aug(1,:) =     level(int(cldfr_idx(:,1))+1)
    X_test_aug(2,:) =  latitude(int(cldfr_idx(:,2))+1)
    X_test_aug(3,:) = longitude(int(cldfr_idx(:,2))+1)
    X_test_aug(4,:) = dble(i)
  end function

 end program infer_aerosol
