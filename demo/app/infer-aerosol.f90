! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

program infer_aerosol

  ! Intrinsic modules:
  use iso_fortran_env, only : int64, real64

  ! External dependencies:
  use fiats_m, only : unmapped_network_t, tensor_t, double_precision, double_precision_file_t
  use julienne_m, only : string_t, command_line_t
  use omp_lib

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

  print *,new_line('') // "______infer-aerosol done _______"

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
    double precision, allocatable, dimension(:,:) :: cldfr_idx, basis, X_test, input_components, output_components
    type(unmapped_network_t(double_precision)) branch_network, trunk_network
    integer i, j

    associate(saved_data_file => NetCDF_file_t(path // saved_data_file_name))
      print '(a)', "Reading saved from " // path // saved_data_file_name
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

    print *, "Reading the branch network from " // branch_file_name
    branch_network = unmapped_network_t(double_precision_file_t(path // branch_file_name))
    print *, "Reading the trunk network from " // trunk_file_name
    trunk_network = unmapped_network_t(double_precision_file_t(path // trunk_file_name))

    allocate(input_components(size(X_test,2),size(X_test,1)))

    pre_process: &
    do concurrent(i = 1:size(X_test,1),  j = 1:size(X_test,2)) default(none) shared(X_test,input_components)
      associate(cube_root => (abs(X_test(i,j))**(1.d0/3.d0))*sign(1.d0,X_test(i,j)))
        input_components(j,i) = (cube_root - mean_X(i))/std_X(i)
      end associate
    end do pre_process

    stop "----> done < ----"

    !allocate(output_components(size(aerosol_data,2),num_outputs))

    !time_inference: &
    !block
    !  integer(int64) t_start, t_finish, clock_rate
    !  type(tensor_t(double_precision)), allocatable :: inputs(:), outputs(:)
    !  double precision start_time,end_time
    !  real, allocatable :: output_slice(:)
    !  integer i, icc

    !  allocate(inputs(size(input_components,1)))
    !  allocate(outputs(size(input_components,1)))      

    !  !$omp parallel do shared(inputs,input_components) 
    !  do i = 1,size(input_components,1)
    !    inputs(i) = tensor_t(input_components(i,:))
    !  end do
    !  !$omp end parallel do         

    !  print*, "Starting inference."
    !  call system_clock(t_start, clock_rate)      

    !  icc = size(input_components,1)

    !  !$ start_time = omp_get_wtime()
    !  !$omp parallel do shared(inputs,outputs,icc)
    !  do i = 1,icc
    !     outputs(i) = neural_network%infer(inputs(i))
    !  end do
    !  !$omp end parallel do   
    !  !$    end_time = omp_get_wtime()       

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

    !end block time_inference
   
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

end program infer_aerosol