! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program concurrent_inferences
  !! This program demonstrates how to read a neural network from a JSON file
  !! and use the network to perform concurrent inferences.
  use fiats_m, only : neural_network_t, tensor_t, double_precision, double_precision_string_t, double_precision_file_t
  use julienne_m, only : string_t, command_line_t, file_t
  use assert_m, only : assert
  use iso_fortran_env, only : int64, real64
  use omp_lib
  implicit none

  type(string_t) network_file_name
  type(command_line_t) command_line
  type(neural_network_t) neural_network
  type(tensor_t), allocatable :: inputs(:,:,:), outputs(:,:,:)
  integer, parameter :: lat=263, lev=15, lon=317 ! latitudes, levels (elevations), longitudes

  network_file_name = string_t(command_line%flag_value("--network"))

  if (len(network_file_name%string())==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: fpm run --example concurrent-inferences --profile release --flag "-fopenmp" -- --network "<file-name>"'
  end if

  inputs = random_inputs()
  allocate(outputs, mold=inputs)

  call do_concurrent_inference
  call openmp_inference
  call elemental_inference
  call double_precision_do_concurrent_inference

contains

  function random_inputs()
    real, allocatable :: input_components(:,:,:,:)
    type(tensor_t), allocatable :: random_inputs(:,:,:)
    integer i, k, j

    print *, "Constructing a new neural_network_t object from the file " // network_file_name%string()
    neural_network = neural_network_t(file_t(network_file_name))

    print *,"Defining an array of tensor_t input objects with random normalized components"
    allocate(random_inputs(lat,lev,lon))
    allocate(input_components(lat,lev,lon,neural_network%num_inputs()))
    call random_number(input_components)

    do concurrent(i=1:lat, k=1:lev, j=1:lon)
      random_inputs(i,k,j) = tensor_t(input_components(i,k,j,:))
    end do
  end function

  subroutine do_concurrent_inference
    integer(int64) t_start, t_finish, clock_rate
    integer i, k, j

    print *,"Performing",lat*lev*lon," inferences inside `do concurrent`."
    call system_clock(t_start, clock_rate)
    do concurrent(i=1:lat, k=1:lev, j=1:lon)
      outputs(i,k,j) = neural_network%infer(inputs(i,k,j))
    end do
    call system_clock(t_finish)
    print *,"Elapsed system clock during inference: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  end subroutine

  subroutine openmp_inference
    integer(int64) t_start, t_finish, clock_rate
    integer i, k, j

    print *,"Performing",lat*lev*lon," inferences inside `omp parallel do`."
    call system_clock(t_start, clock_rate)
    !$omp parallel do default(none) shared(neural_network,inputs,outputs) collapse(3)
    do j=1,lon
      do k=1,lev
        do i=1,lat
          outputs(i,k,j) = neural_network%infer(inputs(i,k,j))
        end do
      end do
    end do
    call system_clock(t_finish)
    print *,"Elapsed system clock during inference: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  end subroutine

  subroutine elemental_inference
    integer(int64) t_start, t_finish, clock_rate

    print *,"Performing elemental inferences inside `omp workshare`"
    call system_clock(t_start, clock_rate)
    !$omp workshare
    outputs = neural_network%infer(inputs)
    !$omp end workshare
    call system_clock(t_finish)
    print *,"Elapsed system clock during inference: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  end subroutine

  subroutine double_precision_do_concurrent_inference
    integer(int64) t_start, t_finish, clock_rate
    integer i, k, j
    type(neural_network_t(double_precision)) neural_network
    type(tensor_t(double_precision)), allocatable :: inputs(:,:,:), outputs(:,:,:)
    double precision, allocatable :: input_components(:,:,:,:)

    print *, "Constructing a new neural_network_t object from the file " // network_file_name%string()
    neural_network = neural_network_t(double_precision_file_t(network_file_name))

    print *,"Defining an array of tensor_t input objects with random normalized components"
    allocate(outputs(lat,lev,lon))
    allocate( inputs(lat,lev,lon))
    allocate(input_components(lat,lev,lon,neural_network%num_inputs()))
    call random_number(input_components)

    do concurrent(i=1:lat, k=1:lev, j=1:lon)
      inputs(i,k,j) = tensor_t(input_components(i,k,j,:))
    end do

    print *,"Performing double-precision inference inside `do concurrent`"
    call system_clock(t_start, clock_rate)
    do concurrent(i=1:lat, k=1:lev, j=1:lon)
      outputs(i,k,j) = neural_network%infer(inputs(i,k,j))
    end do
    call system_clock(t_finish)
    print *,"Elapsed system clock during inference: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  end subroutine

end program
