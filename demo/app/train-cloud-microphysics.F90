! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program train_cloud_microphysics
  !! Train a neural network to represent the simplest cloud microphysics model from
  !! the Intermediate Complexity Atmospheric Research Model (ICAR) at
  !! https://github.com/BerkeleyLab/icar.

  !! Intrinic modules :
  use iso_fortran_env, only : int64, real64

  !! External dependencies:
  use julienne_m, only : string_t, file_t, command_line_t, bin_t
  use assert_m, only : assert, intrinsic_array_t
  use fiats_m, only : &
    neural_network_t, mini_batch_t, input_output_pair_t, tensor_t, trainable_network_t, tensor_map_t, training_configuration_t, &
    shuffle
    
  !! Internal dependencies:
  use phase_space_bin_m, only : phase_space_bin_t
  use NetCDF_file_m, only: NetCDF_file_t
  use NetCDF_variable_m, only: NetCDF_variable_t, tensors
  use occupancy_m, only : occupancy_t
  implicit none

  character(len=*), parameter :: usage =                                                        new_line('a') // new_line('a') // &
    'Usage: ' //                                                                                new_line('a') // new_line('a') // &
    './build/run-fpm.sh run train-cloud-microphysics -- \'                                                    // new_line('a') // &
    '  --base <string> --epochs <integer> \'                                                                  // new_line('a') // &
    '  [--start <integer>] [--end <integer>] [--stride <integer>] [--bins <integer>] [--report <integer>] [--tolerance <real>]'// &
                                                                                                new_line('a') // new_line('a') // &
    'where angular brackets denote user-provided values and square brackets denote optional arguments.'       // new_line('a') // &
    'The presence of a file named "stop" halts execution gracefully.'

  type command_line_arguments_t 
    integer num_epochs, start_step, stride, num_bins, report_step
    integer, allocatable :: end_step
    character(len=:), allocatable :: base_name
    real cost_tolerance
  end type

  type plot_file_t
    character(len=:), allocatable :: file_name
    integer plot_unit, previous_epoch
  end type

  integer(int64) t_start, t_finish, clock_rate
  character(len=*), parameter :: config= "training_configuration.json"

  call system_clock(t_start, clock_rate)
  
#if defined(MULTI_IMAGE_SUPPORT)
  if (this_image()==1) then
#endif
    call read_train_write(training_configuration_t(file_t(config)), get_command_line_arguments(), create_or_append_to("cost.plt"))
#if defined(MULTI_IMAGE_SUPPORT)
  else
    call read_train_write(training_configuration_t(file_t(config)), get_command_line_arguments())
  end if
#endif

  call system_clock(t_finish)

  print *,"System clock time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  print *,new_line('a') // "______train_cloud_microhpysics done _______"

contains

  function create_or_append_to(plot_file_name) result(plot_file)
    type(plot_file_t) plot_file
    character(len=*), intent(in) :: plot_file_name
    integer plot_unit, previous_epoch
    logical preexisting_plot_file

    inquire(file=plot_file_name, exist=preexisting_plot_file)

    if (.not. preexisting_plot_file) then
      open(newunit=plot_unit, file=plot_file_name, status="new", action="write")
      write(plot_unit,*) "      Epoch  Cost (avg)"
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
      base_name, epochs_string, start_string, end_string, stride_string, bins_string, report_string, tolerance_string
    real cost_tolerance
    integer, allocatable :: end_step
    integer num_epochs, num_bins, start_step, stride, report_step

    base_name = command_line%flag_value("--base") ! gfortran 13 seg faults if this is an association
    epochs_string = command_line%flag_value("--epochs")
    start_string = command_line%flag_value("--start")
    end_string = command_line%flag_value("--end")
    stride_string = command_line%flag_value("--stride")
    bins_string = command_line%flag_value("--bins")
    report_string = command_line%flag_value("--report")
    tolerance_string = command_line%flag_value("--tolerance")

    associate(required_arguments => len(base_name)/=0 .and. len(epochs_string)/=0)
       if (.not. required_arguments) error stop usage 
    end associate

    read(epochs_string,*) num_epochs

    stride         = default_integer_or_read(1,    stride_string)
    start_step     = default_integer_or_read(1,     start_string)
    report_step    = default_integer_or_read(1,    report_string)
    num_bins       = default_integer_or_read(3,      bins_string)
    cost_tolerance = default_real_or_read(5E-8, tolerance_string)

    if (len(end_string)/=0) then
      allocate(end_step)
      read(end_string,*) end_step
    end if
 
    if (allocated(end_step)) then 
      command_line_arguments = command_line_arguments_t( &
        num_epochs, start_step, stride, num_bins, report_step, end_step, base_name, cost_tolerance &
      )
    else
      command_line_arguments = command_line_arguments_t( &
        num_epochs, start_step, stride, num_bins, report_step, null(), base_name, cost_tolerance &
      )
    end if
    
  end function get_command_line_arguments

  subroutine read_train_write(training_configuration, args, plot_file)
    type(training_configuration_t), intent(in) :: training_configuration
    type(command_line_arguments_t), intent(in) :: args
    type(plot_file_t), intent(in), optional :: plot_file 
    type(NetCDF_variable_t), allocatable :: input_variable(:), output_variable(:), derivative(:)
    type(NetCDF_variable_t) input_time, output_time

    ! local variables:
    type(trainable_network_t) trainable_network
    type(mini_batch_t), allocatable :: mini_batches(:)
    type(bin_t), allocatable :: bins(:)
    type(input_output_pair_t), allocatable :: input_output_pairs(:)
    type(tensor_t), allocatable, dimension(:) :: input_tensors, output_tensors
    real, allocatable :: cost(:)
    integer i, network_unit, io_status, epoch, end_step, t, b, t_end, v
    integer(int64) start_training, finish_training
    logical stop_requested

   input_names: &
    associate(input_names => &
      [string_t("pressure"), string_t("potential_temperature"), string_t("temperature"), &
       string_t("qv"), string_t("qc"), string_t("qr"), string_t("qs")] &
    )
      allocate(input_variable(size(input_names)))

      input_file_name: &
      associate(input_file_name => args%base_name // "_input.nc")

        print *,"Reading physics-based model inputs from " // input_file_name 

        input_file: &
        associate(input_file => netCDF_file_t(input_file_name))

          do v=1, size(input_variable) 
            print *,"- reading ", input_names(v)%string()
            call input_variable(v)%input(input_names(v), input_file, rank=4)
          end do

          do v = 2, size(input_variable)
            call assert(input_variable(v)%conformable_with(input_variable(1)), "train_cloud_microphysics: input variable conformance")
          end do

          print *,"- reading time"
          call input_time%input("time", input_file, rank=1)

        end associate input_file
      end associate input_file_name
    end associate input_names

    output_names: &
    associate(output_names => [string_t("potential_temperature"),string_t("qv"), string_t("qc"), string_t("qr"), string_t("qs")])

      allocate(output_variable(size(output_names)))

      output_file_name: &
      associate(output_file_name => args%base_name // "_output.nc")

        print *,"Reading physics-based model outputs from " // output_file_name 

        output_file: &
        associate(output_file => netCDF_file_t(output_file_name))

          do v=1, size(output_variable)
            print *,"- reading ", output_names(v)%string()
            call output_variable(v)%input(output_names(v), output_file, rank=4)
          end do

          do v = 1, size(output_variable)
            call assert(output_variable(v)%conformable_with(input_variable(1)), "train_cloud_microphysics: output variable conformance")
          end do

          print *,"- reading time"
          call output_time%input("time", output_file, rank=1)

          call assert(output_time%conformable_with(input_time), "train_cloud_microphysics: input/output time conformance")

        end associate output_file
      end associate output_file_name

      print *,"Calculating desired neural-network model outputs"

      allocate(derivative, mold=output_variable)

      dt: &
      associate(dt => NetCDF_variable_t(output_time - input_time, "dt"))
        do v = 1, size(derivative)
          derivative_name: &
          associate(derivative_name => "d" // output_names(v)%string() // "/dt")
            print *,"- " // derivative_name
            derivative(v) = NetCDF_variable_t( input_variable(v) - output_variable(v) / dt, derivative_name)
            call assert(.not. derivative(v)%any_nan(), "train_cloud_microhphysics: non NaN's")
          end associate derivative_name
        end do
      end associate dt
    end associate output_names

    if (allocated(args%end_step)) then
      end_step = args%end_step
    else
      end_step = input_variable(1)%end_step()
    end if

    print *,"Defining input tensors for time step", args%start_step, "through", end_step, "with strides of", args%stride
    input_tensors  = tensors(input_variable,  step_start = args%start_step, step_end = end_step, step_stride = args%stride)

    print *,"Defining output tensors for time step", args%start_step, "through", end_step, "with strides of", args%stride
    output_tensors = tensors(derivative, step_start = args%start_step, step_end = end_step, step_stride = args%stride)

    output_map: &
    associate(                    &
      output_map => tensor_map_t( &
         layer    = "outputs"     &
        ,minima   = [( derivative(v)%minimum(), v=1, size(derivative) )] &
        ,maxima   = [( derivative(v)%maximum(), v=1, size(derivative) )] &
    ))
      train_network: &
      block
        
        network_file: &
        associate(network_file => args%base_name // "_network.json")
        
          open(newunit=network_unit, file=network_file, form='formatted', status='old', iostat=io_status, action='read')
          
          read_or_initialize_network:   &
          if (io_status==0) then
            print *,"Reading network from file " // network_file
            trainable_network = trainable_network_t(neural_network_t(file_t(string_t(network_file))))
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
                  ,input_map  = tensor_map_t(            &
                     layer    = "inputs"                &
                    ,minima   = [( input_variable(v)%minimum(),  v=1, size( input_variable) )] &
                    ,maxima   = [( input_variable(v)%maximum(),  v=1, size( input_variable) )] &
                  )                        &
                  ,output_map = output_map &
                )
              end associate activation
            end block initialize_network

          end if read_or_initialize_network

          print *, "Conditionally sampling for a flat distribution of output values"

          flatten_histogram: &
          block
            integer i
            !logical occupied(args%num_bins, args%num_bins, args%num_bins, args%num_bins, args%num_bins)
            logical occupied(args%num_bins, args%num_bins)
            logical keepers(size(output_tensors))
            type(phase_space_bin_t), allocatable :: bin(:)
            type(occupancy_t) occupancy

            ! Determine the phase-space bin that holds each output tensor
            bin = [(phase_space_bin_t(output_tensors(i), output_map%minima(), output_map%maxima(), args%num_bins), i = 1, size(output_tensors))]
            call occupancy%vacate( dims = [( args%num_bins, i = 1, size(output_variable))] )

            keepers = .false.

            do i = 1, size(output_tensors)
              if (occupancy%occupied(bin(i)%loc)) cycle
              call occupancy%occupy(bin(i)%loc)
              keepers(i) = .true.
            end do
            input_output_pairs = input_output_pair_t(pack(input_tensors, keepers), pack(output_tensors, keepers))
            print '(*(a,i))' &
             ," Keeping "              , size(input_output_pairs, kind=int64) &
             ," out of "               , size(output_tensors, kind=int64)            &
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


#if defined(MULTI_IMAGE_SUPPORT)
          me = this_image()
#else
          me = 1
#endif
            if (me==1) first_epoch = plot_file%previous_epoch + 1

#if defined(MULTI_IMAGE_SUPPORT)
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

                      print *, epoch, average_cost
                      write(plot_file%plot_unit,*) epoch, average_cost

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

        end associate network_file

        call system_clock(finish_training)
        print *,"Training time: ", real(finish_training - start_training, real64)/real(clock_rate, real64),"for", &
          args%num_epochs,"epochs"

      !  end associate 
      end block train_network

    end associate output_map

    !close(plot_file%plot_unit)

  end subroutine read_train_write

  pure function default_integer_or_read(default, string) result(set_value)
    integer, intent(in) :: default
    character(len=*), intent(in) :: string
    integer set_value
    
    if (len(string)==0) then
      set_value = default
    else
      read(string,*) set_value
    end if

  end function

  pure function default_real_or_read(default, string) result(set_value)
    real, intent(in) :: default
    character(len=*), intent(in) :: string
    real set_value
    
    if (len(string)==0) then
      set_value = default
    else
      read(string,*) set_value
    end if

  end function

end program train_cloud_microphysics
