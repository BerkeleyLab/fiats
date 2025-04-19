! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

program tensor_statistics
  !! This program
  !! 1. Computes the ranges and histograms of input and output tensors saved by
  !!    the neural-net branch of the Berkeley Lab fork of [ICAR](https://go.lbl.gov/icar).
  !! 2. Saves the resulting statistics to text files with space-separated columns and column labels.

  ! External dependencies:
  use julienne_m, only : command_line_t, file_t, string_t
  use assert_m
  use ieee_arithmetic, only : ieee_is_nan
  use iso_fortran_env, only : int64, real64
    
  ! Internal dependencies:
  use NetCDF_file_m, only: NetCDF_file_t
  use NetCDF_variable_m, only: NetCDF_variable_t, time_derivative_t
  use histogram_m, only: histogram_t, to_file
  use time_data_m, only: time_data_t, icar_output_file_t
  use fiats_m, only : training_configuration_t, training_data_files_t
  implicit none

  character(len=*), parameter :: usage =                new_line('') // new_line('') // & 
    'Usage: '                                        // new_line('') // new_line('') // &
    './build/run-fpm.sh run tensor-statistics -- --bins <integer> \' // new_line('') // &
    '  [--raw] [--start <integer>] [--end <integer>] [--stride <integer>]'           // &
                                                        new_line('') // new_line('') // &
    'where angular brackets denote user-provided values and square brackets denote optional arguments.' // new_line('')

  integer(int64) t_start, t_finish, clock_rate
  integer num_bins, start_step, stride
  integer, allocatable :: end_step
  logical raw
  type(string_t), allocatable :: input_file_names(:), output_file_names(:)

  call system_clock(t_start, clock_rate)
  call get_command_line_arguments(num_bins, start_step, end_step, stride, raw)

  associate( &
     training_configuration => training_configuration_t(file_t("training_configuration.json")) &
    ,training_data_files => training_data_files_t(file_t("training_data_files.json")) &
  )

    call compute_histograms( &
       input_tensor_file_names  = training_data_files%fully_qualified_inputs_files() &
      ,output_tensor_file_names = training_data_files%fully_qualified_outputs_files() &
      ,time_data_file_name      = training_data_files%fully_qualified_time_file() &
      ,input_component_names    = training_configuration%input_variable_names() &
      ,output_component_names   = training_configuration%output_variable_names() &
      ,raw = raw &
    )
  end associate
  call system_clock(t_finish)

  print '(a,g0)',"System clock time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  print *
  print '(a)',"______________________________________________________"
  print '(a)',"The *.plt files contain tensor ranges and histograms."
  print '(a)',"If you have gnuplot installed, please execute the"
  print '(a)',"following command to produce histogram visualizations:"
  print *
  associate(file_name => trim(merge("plot-raw-histograms       ", "plot-normalized-histograms", raw)) // ".gnu")
   print*,"  gnuplot app/" // file_name
  end associate
  print *
  print '(a)',"_______________ tensor_statistics done________________"

contains

  subroutine get_command_line_arguments(num_bins, start_step, end_step, stride, raw)
    integer, intent(out) :: num_bins, start_step, stride
    integer, intent(out), allocatable :: end_step
    logical, intent(out) :: raw

    ! local variables
    type(command_line_t) command_line
    character(len=:), allocatable :: stride_string, bins_string, start_string, end_string

    bins_string = command_line%flag_value("--bins")
    start_string = command_line%flag_value("--start")
    end_string = command_line%flag_value("--end")
    stride_string = command_line%flag_value("--stride")
    raw = command_line%argument_present(["--raw"])

    associate(required_arguments => len(bins_string)/=0)
       if (.not. required_arguments) error stop usage 
    end associate

    read(bins_string,*) num_bins

    if (len(stride_string)==0) then
      stride = 1
    else
      read(stride_string,*) stride
    end if

    if (len(start_string)==0) then
      start_step = 1
    else
      read(start_string,*) start_step
    end if

    if (len(end_string)/=0) then
      if (.not. allocated(end_step)) allocate(end_step)
      read(end_string,*) end_step
    end if
 
  end subroutine get_command_line_arguments

  subroutine compute_histograms( &
    input_tensor_file_names, output_tensor_file_names, input_component_names, output_component_names, time_data_file_name, raw &
  )
    type(string_t), intent(in) :: input_tensor_file_names(:), output_tensor_file_names(:)
    type(string_t), intent(in) :: input_component_names(:),   output_component_names(:), time_data_file_name
    logical, intent(in) :: raw

    type(time_derivative_t), allocatable :: derivative(:)
    type(NetCDF_variable_t), allocatable :: input_variable(:), output_variable(:)
    type(NetCDF_variable_t) input_time, output_time
    double precision, allocatable, dimension(:) :: dt
    double precision, parameter :: tolerance = 1.E-07
    integer(int64) t_histo_start, t_histo_finish 
    integer t, t_end, v

    allocate(input_variable(size(input_component_names)))

    print '(a)',"Reading physics-based model inputs from " // input_tensor_file_names(1)%string()

    input_file: &
    associate(NetCDF_file => netCDF_file_t(input_tensor_file_names(1)))

      do v=1, size(input_variable) 
        print '(a)',"- reading " // input_component_names(v)%string()
        call input_variable(v)%input(input_component_names(v), NetCDF_file, rank=4)
      end do

      do v = 2, size(input_variable)
        call_assert(input_variable(v)%conformable_with(input_variable(1)))
      end do

      print '(a)',"- reading time from NetCDF file"
      call input_time%input("time", NetCDF_file, rank=1)

    end associate input_file

    print '(a)',"Calculating input tensor histograms."
    call system_clock(t_histo_start)
    associate(histograms => [(input_variable(v)%histogram(num_bins, raw), v = 1, size(input_variable))])
      call system_clock(t_histo_finish)
      print '(i0,a,g0,a)',size(histograms), " input histograms done in ", real(t_histo_finish - t_histo_start, real64)/real(clock_rate, real64), " sec."

      print '(a)',"Writing input tensor histograms file"
      block
        type(file_t) histograms_file
        integer h

        if (raw) then
          do h = 1, size(histograms)
            histograms_file = to_file(histograms(h))
            associate(gnuplot_file_name => histograms(h)%variable_name() // ".plt")
              print '(a)',"- writing " // gnuplot_file_name
              call histograms_file%write_lines(string_t(gnuplot_file_name))
            end associate
          end do
        else
          histograms_file = to_file(histograms)
          associate(gnuplot_file_name => "inputs_stats.plt")
            print '(a)',"- writing " // gnuplot_file_name
            call histograms_file%write_lines(string_t(gnuplot_file_name))
          end associate
        end if
      end block
    end associate

    allocate(output_variable(size(output_component_names)))

    print '(a)',"Reading physics-based model outputs from " // output_tensor_file_names(1)%string()

    output_file: &
    associate(NetCDF_file => netCDF_file_t(output_tensor_file_names(1)))

      do v=1, size(output_variable)
        print '(a)', "- reading " // output_component_names(v)%string()
        call output_variable(v)%input(output_component_names(v), NetCDF_file, rank=4)
      end do

      do v = 2, size(output_variable)
        call_assert(output_variable(v)%conformable_with(output_variable(1)))
      end do

      print '(a)',"- reading time"
      call output_time%input("time", NetCDF_file, rank=1)

    end associate output_file

    print '(a)',"Calculating the desired neural-network model outputs: time derivatives of a subset of the inputs"

    allocate(derivative(size(output_variable)))

    print '(a)',"- reading time from JSON file"
    associate(time_data => time_data_t(file_t(time_data_file_name)))
      do v = 1, size(derivative)
        derivative_name: &
        associate(derivative_name => "d" // output_component_names(v)%string() // "_dt")
          print '(a)',"- " // derivative_name
          derivative(v) = time_derivative_t(old = input_variable(v), new = output_variable(v), dt=time_data%dt())
          call_assert(.not. derivative(v)%any_nan())
        end associate derivative_name
      end do
    end associate

    print '(a)',"Calculating histograms for the desired neural-network outputs."
    call system_clock(t_histo_start)
    associate(histograms => [(derivative(v)%histogram(num_bins, raw), v = 1, size(derivative))])
      call system_clock(t_histo_finish)
      print '(i0,a,g0,a)',size(histograms), " output histograms done in ", real(t_histo_finish - t_histo_start, real64)/real(clock_rate, real64), " sec."

      print '(a)',"Writing desired-output tensor histograms file"
      block
        type(file_t) histograms_file
        integer h

        if (raw) then
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
