! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "fiats-language-support.F90"
#include "julienne-assert-macros.h"

program train_cloud_microphysics
  !! Train a neural network to represent a cloud microphysics model from
  !! [ICAR](https://go.lbl.gov/icar))

  !! Intrinsic modules :
  use iso_fortran_env, only : int64, real64

  !! External dependencies:
  use julienne_m, only : string_t, file_t, command_line_t, bin_t, call_julienne_assert_
  use fiats_m, only : tensor_t, trainable_network_t, input_output_pair_t, mini_batch_t, &
    tensor_map_t, training_configuration_t, training_data_files_t, shuffle

  !! Internal dependencies:
  use phase_space_bin_m, only : phase_space_bin_t
  use NetCDF_file_m, only: NetCDF_file_t
  use NetCDF_variable_m, only: NetCDF_variable_t, tensors, time_derivative_t
  use occupancy_m, only : occupancy_t
  use default_m, only: default_or_internal_read
  use time_data_m, only: time_data_t

  implicit none

  character(len=*), parameter :: usage =                                                          new_line('') // new_line('') // &
    'Usage: ' //                                                                                  new_line('') // new_line('') // &
    './build/run-fpm.sh run train-cloud-microphysics -- \'                                                     // new_line('') // &
    '  --base <string> --epochs <integer> [--bins <integer>] [--report <integer>] [--tolerance <real>] '       // new_line('') // &
                                                                                                  new_line('') // new_line('') // &
    'where angular brackets denote user-provided values and square brackets denote optional arguments.'        // new_line('') // &
    'The presence of a file named "stop" halts execution gracefully.'                                          // new_line('')

  type command_line_arguments_t 
    integer num_epochs, num_bins, report_step
    character(len=:), allocatable :: base_name
    real cost_tolerance
  end type

  type plot_file_t
    character(len=:), allocatable :: file_name
    integer plot_unit, previous_epoch
  end type

  integer(int64) t_start, t_finish, clock_rate

  call system_clock(t_start, clock_rate)
  associate( &
     training_configuration => training_configuration_t(file_t("training_configuration.json")) &
    ,training_data_files => training_data_files_t(file_t("training_data_files.json")) &
  )
#if defined(FIATS_MULTI_IMAGE_SUPPORT)
    if (this_image()==1) then
#endif
      call read_train_write(training_configuration, training_data_files, get_command_line_arguments(), create_or_append_to("cost.plt"))
#if defined(FIATS_MULTI_IMAGE_SUPPORT)
    else
      call read_train_write(training_configuration, training_data_files, get_command_line_arguments())
    end if
#endif
  end associate

  call system_clock(t_finish)

  print *,"System clock time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  print *,new_line('a') // "______train_cloud_microphysics done _______"

contains

  function create_or_append_to(plot_file_name) result(plot_file)
    type(plot_file_t) plot_file
    character(len=*), intent(in) :: plot_file_name
    integer plot_unit, previous_epoch
    logical preexisting_plot_file

    inquire(file=plot_file_name, exist=preexisting_plot_file)

    if (.not. preexisting_plot_file) then
      open(newunit=plot_unit, file=plot_file_name, status="new", action="write")
      write(plot_unit,'(a)') "      Epoch  Cost (avg)"
      previous_epoch = 0
    else
      associate(plot_file => file_t(string_t(plot_file_name)))
        associate(lines => plot_file%lines())
          associate(num_lines => size(lines))
            if (num_lines == 0 .or. num_lines == 1) then
              previous_epoch = 0
            else
              block
                character(len=:), allocatable :: last_line
                last_line = lines(size(lines))%string()
                read(last_line,*) previous_epoch
              end block
            end if
          end associate
        end associate
      end associate
    end if

    plot_file = plot_file_t(plot_file_name, plot_unit, previous_epoch)

  end function

  function get_command_line_arguments() result(command_line_arguments)
    type(command_line_arguments_t) command_line_arguments
    type(command_line_t) command_line
    character(len=:), allocatable :: &
      base_name, epochs_string, bins_string, report_string, tolerance_string
    real cost_tolerance
    integer num_epochs, num_bins, report_step

    base_name = command_line%flag_value("--base")
    epochs_string = command_line%flag_value("--epochs")
    bins_string = command_line%flag_value("--bins")
    report_string = command_line%flag_value("--report")
    tolerance_string = command_line%flag_value("--tolerance")

    associate(required_arguments => len(base_name)/=0 .and. len(epochs_string)/=0)
       if (.not. required_arguments) error stop usage 
    end associate

    read(epochs_string,*) num_epochs

    report_step    = default_or_internal_read(1,    report_string)
    num_bins       = default_or_internal_read(3,      bins_string)
    cost_tolerance = default_or_internal_read(5E-8, tolerance_string)

    command_line_arguments = command_line_arguments_t(num_epochs, num_bins, report_step, base_name, cost_tolerance)

  end function get_command_line_arguments

  subroutine read_train_write(training_configuration, training_data_files, args, plot_file)
    type(training_configuration_t), intent(in) :: training_configuration
    type(training_data_files_t), intent(in) :: training_data_files
    type(command_line_arguments_t), intent(in) :: args
    type(plot_file_t), intent(in), optional :: plot_file 
    type(time_derivative_t), allocatable, dimension(:,:) :: derivative
    type(NetCDF_variable_t), allocatable, dimension(:,:) :: input_variable, output_variable
    type(NetCDF_variable_t) input_time, output_time
    type(NetCDF_file_t)    , allocatable, dimension(:)   :: NetCDF_input_file, NetCDF_output_file

    ! local variables:
    type(trainable_network_t) trainable_network
    type(mini_batch_t), allocatable :: mini_batches(:)
    type(bin_t), allocatable :: bins(:)
    type(input_output_pair_t), allocatable :: input_output_pairs(:)
    type(tensor_t), allocatable, dimension(:) :: input_tensors, output_tensors
    real, allocatable :: cost(:)
    integer f, v, network_unit, io_status, epoch, t, b, t_end
    integer(int64) start_training, finish_training
    logical stop_requested

    input_variable_files: &
    associate( &
       input_tensor_file_names  => training_data_files%fully_qualified_inputs_files() &
      ,input_component_names    => training_configuration%input_variable_names() &
    ) 
      allocate(NetCDF_input_file(size(input_tensor_file_names)))
      allocate(input_variable(size(input_component_names), size(NetCDF_input_file)))

      count_files_and_variables: &
      associate(num_input_files => size(NetCDF_input_file), num_variables => size(input_variable,1))

        read_input_files: &
        do f = 1, num_input_files

          print '(a)',"Reading physics-based model inputs from " // input_tensor_file_names(f)%string()
          NetCDF_input_file(f) = netCDF_file_t(input_tensor_file_names(f))

          read_variables: &
          do v = 1, num_variables
            print '(a)',"- reading " // input_component_names(v)%string() // " from " // input_tensor_file_names(f)%string()
            call input_variable(v,f)%input(input_component_names(v), NetCDF_input_file(f), rank=4)
            call_julienne_assert(input_variable(v,f)%conformable_with(input_variable(1,f)))
          end do read_variables

        end do read_input_files

      end associate count_files_and_variables
    end associate input_variable_files

    output_variable_and_time_files: &
    associate( &
       output_tensor_file_names => training_data_files%fully_qualified_outputs_files() &
      ,output_component_names   => training_configuration%output_variable_names() &
      ,time_data_file_name      => training_data_files%fully_qualified_time_file() &
    ) 
      allocate(NetCDF_output_file(size(output_tensor_file_names)))
      allocate(output_variable(size(output_component_names), size(NetCDF_output_file)))

      output_file_and_variable_count: &
      associate(num_output_files => size(NetCDF_output_file), num_output_variables => size(output_variable,1))

        print '(a)',"- reading time from JSON file"
        read_times: &
        associate(time_data => time_data_t(file_t(time_data_file_name)))

          print '(a)',"Calculating the desired neural-network model outputs: time derivatives of the outputs"
          allocate(derivative(num_output_variables, num_output_files))

          read_files: &
          do f = 1, num_output_files

            print '(a)',"Reading physics-based model outputs from " // output_tensor_file_names(f)%string()
            NetCDF_output_file(f) = netCDF_file_t(output_tensor_file_names(f))

            read_variables: &
            do v = 1, num_output_variables

              print '(a)',"- reading " // output_component_names(v)%string() // " from " // output_tensor_file_names(f)%string()
              call output_variable(v,f)%input(output_component_names(v), NetCDF_output_file(f), rank=4)
              call_julienne_assert(output_variable(v,f)%conformable_with(output_variable(1,f)))

              derivative_name: &
              associate(derivative_name => "d" // output_component_names(v)%string() // "_dt")
                print '(a)',"- calculating " // derivative_name
                derivative(v,f) = time_derivative_t(old = input_variable(v,1), new = output_variable(v,1), dt=time_data%dt())
                call_julienne_assert(.not. derivative(v,f)%any_nan())
              end associate derivative_name
            end do read_variables
          end do read_files

        end associate read_times
      end associate output_file_and_variable_count
    end associate output_variable_and_time_files

    associate(num_steps => sum( (input_variable(1,:)%end_step()+1) - input_variable(1,:)%start_step()))
      print *,"Defining input tensors for ", num_steps, "time steps"
    end associate

    input_tensors  = tensors(input_variable)

    associate(num_steps => sum( (derivative(1,:)%end_step()+1) - derivative(1,:)%start_step()))
      print *,"Defining output tensors for ", num_steps, "time steps"
    end associate

    output_tensors = tensors(derivative)

    output_map_and_network_file:  &
    associate(                    &
      output_map => tensor_map_t( &
         layer    = "outputs"     &
        ,minima   = [( [( derivative(v,f)%minimum(), v=1, size(derivative,1) )], f = 1, size(derivative,2) )] &
        ,maxima   = [( [( derivative(v,f)%maximum(), v=1, size(derivative,1) )], f = 1, size(derivative,2) )] &
      ), &
      network_file => args%base_name // "_network.json" &
    )

      check_for_network_file: &
      block
        logical preexisting_network_file

        inquire(file=network_file, exist=preexisting_network_file)

        read_or_initialize_network:   &
        if (preexisting_network_file) then
          print *,"Reading network from file " // network_file
          trainable_network = trainable_network_t(file_t(string_t(network_file)))
          close(network_unit)
        else
          close(network_unit)

          initialize_network: &
          block
            character(len=len('YYYYMMDD')) date

            call date_and_time(date)

            print *,"Defining a new network from training_configuration_t and tensor_map_t objects"

            activation: &
            associate(activation => training_configuration%activation())
              trainable_network = trainable_network_t( &
                 training_configuration                &
                ,perturbation_magnitude = 0.05         &
                ,metadata = [                          &
                   string_t("ICAR microphysics" )      &
                  ,string_t("max-entropy-filter")      &
                  ,string_t(date                )      &
                  ,activation%function_name(    )      &
                  ,string_t(trim(merge("true ", "false", training_configuration%skip_connections()))) &
                ]                                      &
                ,input_map = tensor_map_t(             &
                   layer   = "inputs"                  &
                  ,minima  = [( [( input_variable(v,f)%minimum(), v = 1, size(input_variable,1) )], f = 1, size(input_variable,2) )] &
                  ,maxima  = [( [( input_variable(v,f)%maximum(), v = 1, size(input_variable,1) )], f = 1, size(input_variable,2) )] &
                )                        &
                ,output_map = output_map &
              )
            end associate activation
          end block initialize_network

        end if read_or_initialize_network
      end block  check_for_network_file

      print *, "Conditionally sampling for a flat distribution of output values"

      flatten_histogram: &
      block
        integer i
        logical occupied(args%num_bins, args%num_bins)
        type(phase_space_bin_t), allocatable :: bin(:)
        type(occupancy_t) occupancy
#if !defined(__flang__)
        logical keepers(size(output_tensors))
        keepers = .false.
#else
        logical, allocatable :: keepers(:)
        allocate(keepers(size(output_tensors)), source = .false.)
#endif

        print *, "Determine the phase-space bin that holds each output tensor"
        ! Determine the phase-space bin that holds each output tensor
        associate(output_minima => output_map%minima(), output_maxima => output_map%maxima())
          bin = [(phase_space_bin_t(output_tensors(i), output_minima, output_maxima, args%num_bins), i = 1, size(output_tensors))]
        end associate

        call occupancy%vacate( dims = [( args%num_bins, i = 1, size(derivative,1))] )

        print *, "Populate bins"
        do i = 1, size(output_tensors)
          if (occupancy%occupied(bin(i)%loc)) cycle
          call occupancy%occupy(bin(i)%loc)
          keepers(i) = .true.
        end do

        print *, "Pack remaining input/output tensor pairs"
        input_output_pairs = input_output_pair_t(pack(input_tensors, keepers), pack(output_tensors, keepers))

        print '(*(a,i))' &
         ," Keeping "              , size(input_output_pairs, kind=int64) &
         ," out of "               , size(output_tensors, kind=int64)     &
         ," input/output pairs in ", occupancy%num_occupied()             &
         ," out of "               , occupancy%num_bins()                 &
         ," bins."

      end block flatten_histogram

      print *,"Normalizing the remaining input and output tensors"
      input_output_pairs = trainable_network%map_to_training_ranges(input_output_pairs)

      training_parameters: &
      associate( &
        num_pairs => size(input_output_pairs), &
        n_bins => training_configuration%mini_batches(), &
        adam => merge(.true., .false., training_configuration%optimizer_name() == "adam"), &
        learning_rate => training_configuration%learning_rate() &
      )
        bins = [(bin_t(num_items=num_pairs, num_bins=n_bins, bin_number=b), b = 1, n_bins)]

        print *,"Training network"
        print *, "       Epoch  Cost (avg)"

        call system_clock(start_training)

        train_write_and_maybe_exit: &
        block
          integer first_epoch
          integer me
#if defined(FIATS_MULTI_IMAGE_SUPPORT)
          me = this_image()
#else
          me = 1
#endif
          if (me==1) first_epoch = plot_file%previous_epoch + 1
#if defined(FIATS_MULTI_IMAGE_SUPPORT)
          call co_broadcast(first_epoch, source_image=1)
#endif
          last_epoch: &
          associate(last_epoch => first_epoch + args%num_epochs - 1)
            epochs: &
            do epoch = first_epoch, last_epoch

              if (size(bins)>1) call shuffle(input_output_pairs) ! set up for stochastic gradient descent
              mini_batches = [(mini_batch_t(input_output_pairs(bins(b)%first():bins(b)%last())), b = 1, size(bins))]

              call trainable_network%train(mini_batches, cost, adam, learning_rate)

              average_cost: &
              associate(average_cost => sum(cost)/size(cost))
                converged: &
                associate(converged => average_cost <= args%cost_tolerance)

                  image_1_maybe_writes: &
                  if (me==1 .and. any([converged, epoch==[first_epoch,last_epoch], mod(epoch,args%report_step)==0])) then

                    print '(*(g0,4x))', epoch, average_cost
                    write(plot_file%plot_unit,'(*(g0,4x))') epoch, average_cost

                    associate(json_file => trainable_network%to_json())
                      call json_file%write_lines(string_t(network_file))
                    end associate

                  end if image_1_maybe_writes

                  signal_convergence: &
                  if (converged) then
                    block
                      integer unit
                      open(newunit=unit, file="converged", status="unknown") ! The train.sh script detects & removes this file.
                      close(unit)
                      exit epochs
                    end block
                  end if signal_convergence
                end associate converged
              end associate average_cost

              inquire(file="stop", exist=stop_requested)

              graceful_exit: &
              if (stop_requested) then
                print *,'Shutting down because a file named "stop" was found.'
                return
              end if graceful_exit

            end do epochs
          end associate last_epoch
        end block train_write_and_maybe_exit

      end associate training_parameters
    end associate output_map_and_network_file

    call system_clock(finish_training)

    print *,"Training time: ", real(finish_training - start_training, real64)/real(clock_rate, real64),"for", &
      args%num_epochs,"epochs"
    close(plot_file%plot_unit)

  end subroutine read_train_write

end program train_cloud_microphysics
