! Copyright (c) 2023-2025, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program concurrent_inferences
  !! This program demonstrates how to read a neural network from a JSON file
  !! and use the network to perform concurrent inferences.
  use fiats_m, only : neural_network_t, tensor_t, double_precision, double_precision_file_t
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
  integer i, num_trials

  network_file_name = string_t(command_line%flag_value("--network"))

  if (len(network_file_name%string())==0) then
    error stop                      new_line('') // new_line('') &
      // 'Usage:'                                // new_line('') &
      // '  fpm run \'                           // new_line('') &
      // '    --example concurrent-inferences \' // new_line('') &
      // '    --compiler flang-new \'            // new_line('') &
      // '    --flag -O3 \'                      // new_line('') &
      // '    -- --network "<file-name>" \'      // new_line('') &
      // '    [--do-concurrent] [--openmp] [--elemental] [--double-precision] [--trials <integer>]' // new_line('') &
      // 'where <> indicates user input and [] indicates an optional argument.'
  end if

  inputs = random_inputs()
  allocate(outputs, mold=inputs)


  associate( &
    run_do_concurrent    => command_line%argument_present(["--do-concurrent"   ]), &
    run_openmp           => command_line%argument_present(["--openmp"         ]), &
    run_elemental        => command_line%argument_present(["--elemental"       ]), &
    run_double_precision => command_line%argument_present(["--double-precision"])  &
  )
    num_trials = trials()

    block
      real(real64) t_dc(num_trials), t_omp(num_trials), t_elem(num_trials), t_dp_dc(num_trials)

      associate(run_all => merge(.false., .true., any([run_do_concurrent,run_openmp,run_elemental,run_double_precision])))

        do i = 1, num_trials
          if (run_all .or. run_do_concurrent   ) t_dc(i)    = do_concurrent_time()
          if (run_all .or. run_openmp          ) t_omp(i)   = openmp_time()
          if (run_all .or. run_elemental       ) t_elem(i)  = elemental_time()
          if (run_all .or. run_double_precision) t_dp_dc(i) = double_precision_do_concurrent_time()
        end do

        print *,"variable          mean           stdev"

        if (run_all .or. run_do_concurrent   ) call print_stats("t_dc    ", t_dc)
        if (run_all .or. run_openmp          ) call print_stats("t_omp   ", t_omp)
        if (run_all .or. run_elemental       ) call print_stats("t_elem  ", t_elem)
        if (run_all .or. run_double_precision) call print_stats("t_dp_dc ", t_dp_dc)

      end associate
    end block
  end associate

contains

  subroutine print_stats(label, x)
    character(len=*), intent(in) :: label
    real(real64), intent(in) :: x(:)
    associate(n => size(x))
      associate(mean => sum(x)/real(n))
        associate(stdev => sum((x-mean)**2)/real(n))
          print *, label, mean, stdev
        end associate
      end associate
    end associate
  end subroutine

  integer function trials()

    associate(trials_string => command_line%flag_value("--trials"))
      if (len(trials_string)==0) then
        trials = 1
      else
        read(trials_string,*) trials 
      end if
    end associate

  end function

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

  real(real64) function do_concurrent_time()
    integer(int64) t_start, t_finish, clock_rate
    integer i, k, j

    print *,"Performing",lat*lev*lon," inferences inside `do concurrent`."
    call system_clock(t_start, clock_rate)
    do concurrent(i=1:lat, k=1:lev, j=1:lon)
      outputs(i,k,j) = neural_network%infer(inputs(i,k,j))
    end do
    call system_clock(t_finish)
    do_concurrent_time = real(t_finish - t_start, real64)/real(clock_rate, real64)
    print *,"Elapsed system clock during `do concurrent` inference: ", do_concurrent_time
  end function

  real(real64) function openmp_time()
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
    openmp_time = real(t_finish - t_start, real64)/real(clock_rate, real64)
    print *,"Elapsed system clock during `OpenMP` inference: ", openmp_time
  end function

  real(real64) function elemental_time()
    integer(int64) t_start, t_finish, clock_rate

    print *,"Performing elemental inferences inside `omp workshare`"
    call system_clock(t_start, clock_rate)
    !$omp workshare
    outputs = neural_network%infer(inputs)
    !$omp end workshare
    call system_clock(t_finish)
    elemental_time = real(t_finish - t_start, real64)/real(clock_rate, real64)
    print *,"Elapsed system clock during `elemental` inference: ", elemental_time
  end function

  real(real64) function double_precision_do_concurrent_time()
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
    double_precision_do_concurrent_time = real(t_finish - t_start, real64)/real(clock_rate, real64)
    print *,"Elapsed system clock during double precision concurrent inference: ", double_precision_do_concurrent_time
  end function

end program
