! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

program infer_aerosol

  ! External dependencies:
  use fiats_m, only : unmapped_network_t, tensor_t, double_precision, double_precision_file_t
  use julienne_m, only : string_t, command_line_t, csv
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
    character(len=*), parameter :: saved_data_file_name = "saved_data.nc"
    double precision, allocatable, dimension(:) :: longitude, latitude, level, ymean, mean_X, std_X, mean_y, std_y
    double precision, allocatable, dimension(:,:) :: basis, X_test, branch_inputs, trunk_inputs
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

    allocate(branch_inputs(size(X_test,2),size(X_test,1)))

    do concurrent(i = 1:size(X_test,1),  j = 1:size(X_test,2)) ! default(none) shared(X_test,input_components)
      associate(cube_root => (abs(X_test(i,j))**(1.d0/3.d0))*sign(1.d0,X_test(i,j)))
        branch_inputs(j,i) = (cube_root - mean_X(i))/std_X(i)
      end associate
    end do

    time_inference: &
    block
      integer(int64) t_start_dc, t_end_dc, clock_rate, t_start_omp, t_end_omp
      type(tensor_t(double_precision)), allocatable :: outputs(:)
      real, allocatable :: output_slice(:)
      integer s

      count_samples: &
      associate(samples => size(branch_inputs,1))

        allocate(outputs(samples))

        print*, "Starting inference via `do concurrent` with ", samples, " samples of size ",size(branch_inputs,2)
        call system_clock(t_start_dc, clock_rate)
        do concurrent(integer :: s = 1:samples) default(none) shared(outputs, branch_network, branch_inputs)
          outputs(s) = branch_network%infer(tensor_t(branch_inputs(s,:)))
        end do
        call system_clock(t_end_dc)
        print *,"Elapsed system clock during `do concurrent`: ", real(t_end_dc - t_start_dc, real64)/real(clock_rate, real64)


        !print*, "Starting inference via `omp parallel do` with ", samples, " samples of size ",size(input_components,2)
        !call system_clock(t_start_omp, clock_rate)
        !!$ t_start_omp = omp_get_wtime()
        !!$omp parallel do shared(outputs,branch_network,input_components)
        !do s = 1, samples
        !  outputs(s) = branch_network%infer(tensor_t(input_components(s,:)))
        !end do
        !!$omp end parallel do
        !!$ t_end_omp = omp_get_wtime()
        !call system_clock(t_end_omp)
        !print *,"Elapsed system clock during `omp parallel do`: ", real(t_end_omp - t_start_omp, real64)/real(clock_rate, real64)

    !  call system_clock(t_finish)
    !  print*, "Finished inference."
    !  print *,"System clock time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
    !  !$    print*,'OMP Total time = ',end_time - start_time

    !  allocate(output_slice(num_outputs))

    !  !$omp parallel do shared(outputs,output_components,icc,output_stats) private(output_slice,i,j)
    !  post_process: &
    !  do i = 1,icc
    !     output_slice = outputs(i)%values()
    !     do j = 1,num_outputs
    !        output_components(i,j) = (output_stats%standard_deviation(j)*output_slice(j)+output_stats%mean(j))**3
    !     end do
    !  end do post_process
    !  !$omp end parallel do

    end associate count_samples

    allocate(trunk_inputs, mold=slice)

    associate(ncol => size(mean_X))
      !print *,"ncol ",ncol
      !print *,"shape(slice) ", shape(slice)
      !print *,"shape(mean_X) ", shape(mean_X)
      !print *,"shape(std_X) ", shape(std_X)
      do concurrent(i = 1:size(slice,1),  j = 1:size(slice,2)) ! default(none) shared(slice,input_components)
        trunk_inputs(i,j) = (slice(i,j) - mean_X(ncol-4+j))/std_X(ncol-4+j)
      end do
    end associate

    print *,"inputs -----------------"
    do s = 1, 5
      print csv, trunk_inputs(s,:)
    end do
    
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
    double precision :: X_test_aug(size(cldfr_idx,1), 4)

    X_test_aug(:,1) =     level(int(cldfr_idx(:,1))+1)
    X_test_aug(:,2) =  latitude(int(cldfr_idx(:,2))+1)
    X_test_aug(:,3) = longitude(int(cldfr_idx(:,2))+1)
    X_test_aug(:,4) = dble(i)
  end function

 end program infer_aerosol