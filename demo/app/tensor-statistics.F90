! Copyright (c), 2022-2025 The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

program tensor_statistics
  !! This program
  !! 1. Computes the ranges and histograms of input and output tensors saved by
  !!    the neural-net branch of the Berkeley Lab fork of [ICAR](https://go.lbl.gov/icar).
  !! 2. Saves the resulting statistics to text files with space-separated columns and column labels.

  ! External dependencies:
  use julienne_m, only : command_line_t, file_t, string_t, call_julienne_assert_
  use ieee_arithmetic, only : ieee_is_nan
  use iso_fortran_env, only : int64, real64
    
  ! Internal dependencies:
  use NetCDF_file_m, only: NetCDF_file_t
  use NetCDF_variable_m, only: NetCDF_variable_t, time_derivative_t
  use histogram_m, only: to_file
  use time_data_m, only: time_data_t, icar_output_file_t
  use fiats_m, only : training_configuration_t, training_data_files_t
  implicit none

  character(len=*), parameter :: usage = new_line('') &
    // new_line('') // 'Usage: ' &
    // new_line('') &
    // new_line('') // './build/run-fpm.sh run tensor-statistics -- --bins <integer> [[--disaggregate] | [--unified]]' &
    // new_line('') &
    // new_line('') // 'where angular brackets (<>) denote user-provided values, square brackets ([]) denote optional' &
    // new_line('') // 'arguments, a pipe (|) separates exclusive alternatives, and flags have the following meanings:' &
    // new_line('') &
    // new_line('') // '--bins         number of histogram bins in which to collect variable values' &
    // new_line('') // '--disaggregate generates one histogram plot file per variable per training-data file' &
    // new_line('') // '--unified      maps all domains to [0,1] on one histogram plot per training-data file' &
    // new_line('')
  integer(int64) t_start, t_finish, clock_rate
  integer num_bins
  character(len=*), parameter :: data_json = "training_data_files.json", configuration_json = "training_configuration.json"
  logical disaggregated, unified

  call system_clock(t_start, clock_rate)
  call get_command_line_arguments(num_bins, disaggregated, unified)
  call compute_histograms(training_data_files_t(file_t(data_json)), training_configuration_t(file_t(configuration_json)), disaggregated)
  call system_clock(t_finish)

  print '(a,g0)',"System clock time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  print *
  print '(a)',"______________________________________________________"
  print '(a)',"The *.plt files contain tensor ranges and histograms."
  print '(a)',"If you have gnuplot installed, please execute a command"
  print '(a)',"command to produce histogram visualizations:"
  print *
  associate(file_name => trim(merge("unified-", "        ", unified)) // "histogram-plot.gnu")
    print*,"  gnuplot app/" // file_name
  end associate
  print *
  print '(a)',"_______________ tensor_statistics done________________"

contains

  subroutine get_command_line_arguments(num_bins, disaggregated, unified)
    integer, intent(out) :: num_bins
    logical, intent(out) :: disaggregated, unified

    ! local variables
    type(command_line_t) command_line
    character(len=:), allocatable :: bins_string

    bins_string = command_line%flag_value("--bins")
    disaggregated = command_line%argument_present(["--disaggregated"])
    unified = command_line%argument_present(["--unified"])

    associate(required_arguments => len(bins_string)/=0)
       if (.not. required_arguments) error stop usage 
    end associate

    read(bins_string,*) num_bins

  end subroutine get_command_line_arguments

  subroutine compute_histograms(training_data_files, training_configuration, disaggregated)
    type(training_data_files_t), intent(in) :: training_data_files
    type(training_configuration_t), intent(in) :: training_configuration
    logical, intent(in) :: disaggregated

    type(time_derivative_t), allocatable :: derivative(:)
    type(NetCDF_variable_t), allocatable :: input_variable(:,:), output_variable(:,:)
    type(NetCDF_file_t), allocatable :: NetCDF_input_file(:), NetCDF_output_file(:)
    double precision, allocatable, dimension(:) :: dt
    double precision, parameter :: tolerance = 1.E-07
    integer(int64) t_histo_start, t_histo_finish 
    integer t, t_end, v, f
    type(string_t), allocatable :: input_tensor_file_names(:), output_tensor_file_names(:)
    type(string_t), allocatable :: input_component_names(:), output_component_names(:)
    character(len=:), allocatable :: time_data_file_name

    input_tensor_file_names  = training_data_files%fully_qualified_inputs_files()
    output_tensor_file_names = training_data_files%fully_qualified_outputs_files()
    time_data_file_name      = training_data_files%fully_qualified_time_file()
    input_component_names    = training_configuration%input_variable_names()
    output_component_names   = training_configuration%output_variable_names()

    allocate(NetCDF_input_file(size(input_tensor_file_names)))
    allocate(input_variable(size(input_component_names), size(NetCDF_input_file)))

    input_variable_histograms: &
    associate(num_input_files => size(NetCDF_input_file), num_variables => size(input_variable,1))

      read_input_variable_files: &
      do f = 1, num_input_files

        print '(a)',"Reading physics-based model inputs from " // input_tensor_file_names(f)%string()
        NetCDF_input_file(f) = netCDF_file_t(input_tensor_file_names(f))

        read_input_variables: &
        do v = 1, num_variables
          print '(a)',"- reading " // input_component_names(v)%string() // " from " // input_tensor_file_names(f)%string()
          call input_variable(v,f)%input(input_component_names(v), NetCDF_input_file(f), rank=4)
          call_julienne_assert(input_variable(v,f)%conformable_with(input_variable(1,f)))
        end do read_input_variables

      end do read_input_variable_files

      print '(a)',"Calculating input tensor histograms."
      call system_clock(t_histo_start, clock_rate)

      compute_histograms: &
      associate( histograms => reshape( &
         [( [(input_variable(v,f)%histogram(num_bins, disaggregated), v = 1, num_variables)], f = 1, num_input_files )] &
        , shape = [num_variables, num_input_files] &
      ))
        call system_clock(t_histo_finish)
        print '(i0,a,g0,a)',size(histograms), &
          " input histograms done in ", real(t_histo_finish - t_histo_start, real64)/real(clock_rate, real64), " sec."

        print '(a)',"Writing input tensor histogram file(s)"

        write_histograms: &
        associate(input_file_names => training_data_files%inputs_files())
          do f = 1, num_input_files
            associate(gnuplot_file_name => input_file_names(f) // "-" // "inputs_stats.plt")
              if (disaggregated) then
                do v = 1, num_variables
                  associate(histograms_file => to_file(histograms(v,f)))
                    associate(variable_file_name => histograms(v,f)%variable_name() // "-" // gnuplot_file_name)
                      print '(a)',"- writing " // variable_file_name%string()
                      call histograms_file%write_lines(variable_file_name)
                    end associate
                  end associate
                end do
              else
                print '(a)',"- writing " // gnuplot_file_name%string()
                associate(histograms_file => to_file(histograms(:,f)))
                  call histograms_file%write_lines(gnuplot_file_name)
                end associate
              end if
            end associate
          end do
        end associate write_histograms
      end associate compute_histograms
    end associate input_variable_histograms

    stop "---> made it"

    ! Currently unused because time stamps were not recorded correctly in the training-data files
    ! type(NetCDF_variable_t) input_time, output_time
    ! print '(a)',"- reading time from NetCDF file"
    ! call input_time%input("time", NetCDF_input_file(1), rank=1)
    ! print '(a)',"- reading time"
    ! call output_time%input("time", NetCDF_output_file(1), rank=1)


    allocate(NetCDF_output_file(size(output_tensor_file_names)))
    allocate(output_variable(size(output_component_names), size(NetCDF_output_file)))

    read_output_variable_files: &
    associate(num_files => size(NetCDF_output_file))
      do f = 1, num_files

        print '(a)',"Reading physics-based model outputs from " // output_tensor_file_names(f)%string()
        NetCDF_output_file(f) = netCDF_file_t(output_tensor_file_names(f))

        read_output_variables: &
        associate(num_variables => size(output_variable,1))
          do v = 1, num_variables
            print '(a)',"- reading " // output_component_names(v)%string() // " from " // output_tensor_file_names(f)%string()
            call output_variable(v,f)%input(output_component_names(v), NetCDF_output_file(f), rank=4)
            call_julienne_assert(output_variable(v,f)%conformable_with(output_variable(1,f)))
          end do 
        end associate read_output_variables

      end do 
    end associate read_output_variable_files

    print '(a)',"Calculating the desired neural-network model outputs: time derivatives of the outputs"

    allocate(derivative(size(output_variable,1)))

    print '(a)',"- reading time from JSON file"
    associate(time_data => time_data_t(file_t(time_data_file_name)))
      do v = 1, size(derivative)
        derivative_name: &
        associate(derivative_name => "d" // output_component_names(v)%string() // "_dt")
          print '(a)',"- " // derivative_name
          derivative(v) = time_derivative_t(old = input_variable(v,1), new = output_variable(v,1), dt=time_data%dt())
          call_julienne_assert(.not. derivative(v)%any_nan())
        end associate derivative_name
      end do
    end associate

    print '(a)',"Calculating histograms for the desired neural-network outputs."
    call system_clock(t_histo_start, clock_rate)
    associate(histograms => [(derivative(v)%histogram(num_bins, disaggregated), v = 1, size(derivative))])
      call system_clock(t_histo_finish)
      print '(i0,a,g0,a)',size(histograms), " output histograms done in ", real(t_histo_finish - t_histo_start, real64)/real(clock_rate, real64), " sec."

      print '(a)',"Writing desired-output tensor histograms file"
      block
        type(file_t) histograms_file
        integer h

        if (disaggregated) then
          do h = 1, size(histograms)
            histograms_file = to_file(histograms(h))
            associate(gnuplot_file_name => histograms(h)%variable_name() // ".plt")
              print '(a)',"- writing " // gnuplot_file_name
              call histograms_file%write_lines(string_t(gnuplot_file_name))
            end associate
          end do
        else
          histograms_file = to_file(histograms)
          associate(gnuplot_file_name => "outputs_stats.plt")
            print '(a)',"- writing " // gnuplot_file_name
            call histograms_file%write_lines(string_t(gnuplot_file_name))
          end associate
        end if
      end block
    end associate

  end subroutine

end program tensor_statistics
