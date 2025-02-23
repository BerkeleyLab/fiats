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
  use NetCDF_variable_m, only: NetCDF_variable_t
  use histogram_m, only: histogram_t, to_file
  use training_configuration_m, only : training_configuration_t
  implicit none

  character(len=*), parameter :: usage =    new_line('a') // new_line('a') // & 
    'Usage: '                            // new_line('a') // new_line('a') // &
    './build/run-fpm.sh run tensor-statistics -- \'       // new_line('a') // &
    '  --base <string> --bins <integer> \'                // new_line('a') // &
    '  [--raw] [--start <integer>] [--end <integer>] [--stride <integer>]' // &
                                            new_line('a') // new_line('a') // &
    'where angular brackets denote user-provided values and square brackets denote optional arguments.' &
                                                          // new_line('a')

  integer(int64) t_start, t_finish, clock_rate
  integer num_bins, start_step, stride
  integer, allocatable :: end_step
  character(len=:), allocatable :: base_name
  logical raw

  call system_clock(t_start, clock_rate)
  call get_command_line_arguments(base_name, num_bins, start_step, end_step, stride, raw)
  associate(training_configuration => training_configuration_t(file_t("training_configuration.json")))
    call compute_histograms( &
        input_tensors_file_name = base_name // "_input.nc" &
      ,output_tensors_file_name = base_name // "_output.nc" &
      ,raw = raw &
      , input_names = training_configuration%input_names() &
      ,output_names = training_configuration%output_names() &
    )
  end associate
  call system_clock(t_finish)

  print *,"System clock time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  print *
  print *,"______________________________________________________"
  print *,"The *.plt files contain tensor ranges and histograms."
  print *,"If you have gnuplot installed, please execute the"
  print *,"following command to produce histogram visualizations:" 
  print *
  associate(file_name => trim(merge("plot-raw-histograms       ", "plot-normalized-histograms", raw)) // ".gnu")
   print*,"  gnuplot app/" // file_name
  end associate
  print *
  print *,"_______________ tensor_statistics done________________"

contains

  subroutine get_command_line_arguments(base_name, num_bins, start_step, end_step, stride, raw)
    character(len=:), allocatable, intent(out) :: base_name
    integer, intent(out) :: num_bins, start_step, stride
    integer, intent(out), allocatable :: end_step
    logical, intent(out) :: raw

    ! local variables
    type(command_line_t) command_line
    character(len=:), allocatable :: stride_string, bins_string, start_string, end_string

    base_name = command_line%flag_value("--base")
    bins_string = command_line%flag_value("--bins")
    start_string = command_line%flag_value("--start")
    end_string = command_line%flag_value("--end")
    stride_string = command_line%flag_value("--stride")
    raw = command_line%argument_present(["--raw"])

    associate(required_arguments => len(base_name)/=0 .and. len(bins_string)/=0)
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

  subroutine compute_histograms(input_tensors_file_name, output_tensors_file_name, raw, input_names, output_names)
    character(len=*), intent(in) :: input_tensors_file_name, output_tensors_file_name
    logical, intent(in) :: raw
    type(string_t), intent(in) :: input_names(:), output_names(:)

    type(NetCDF_variable_t), allocatable :: input_variable(:), output_variable(:), derivative(:)
    type(NetCDF_variable_t) input_time, output_time
    double precision, allocatable, dimension(:) :: time_in, time_out
    double precision, parameter :: tolerance = 1.E-07
    integer(int64) t_histo_start, t_histo_finish 
    integer t, t_end, v

    allocate(input_variable(size(input_names)))

    print *,"Reading physics-based model inputs from " // input_tensors_file_name 

    input_file: &
    associate(NetCDF_file => netCDF_file_t(input_tensors_file_name))

      do v=1, size(input_variable) 
        print *,"- reading ", input_names(v)%string()
        call input_variable(v)%input(input_names(v), NetCDF_file, rank=4)
      end do

      do v = 2, size(input_variable)
        call_assert(input_variable(v)%conformable_with(input_variable(1)))
      end do

      print *,"- reading time"
      call input_time%input("time", NetCDF_file, rank=1)

    end associate input_file

    print *,"Calculating input tensor histograms."
    call system_clock(t_histo_start)
    associate(histograms => [(input_variable(v)%histogram(num_bins, raw), v = 1, size(input_variable))])
      call system_clock(t_histo_finish)
      print *,size(histograms), " input histograms done in ", real(t_histo_finish - t_histo_start, real64)/real(clock_rate, real64), " sec."

      print *,"Writing input tensor histograms file"
      block
        type(file_t) histograms_file
        integer h

        if (raw) then
          do h = 1, size(histograms)
            histograms_file = to_file(histograms(h))
            call histograms_file%write_lines(string_t(histograms(h)%variable_name() // ".plt"))
          end do
        else
            histograms_file = to_file(histograms)
            call histograms_file%write_lines(string_t(base_name // "_inputs_stats.plt"))
        end if
      end block
    end associate

    ! print *,"Calculating time derivatives"
  
    !  allocate(dpt_dt, mold = potential_temperature_out)
    !  allocate(dqv_dt, mold = qv_out)
    !  allocate(dqc_dt, mold = qc_out)
    !  allocate(dqr_dt, mold = qr_out)
    !  allocate(dqs_dt, mold = qs_out)

    !  associate(dt => real(time_out - time_in))
    !    do concurrent(t = 1:t_end)
    !      dpt_dt(:,:,:,t) = (potential_temperature_out(:,:,:,t) - potential_temperature_in(:,:,:,t))/dt(t)
    !      dqv_dt(:,:,:,t) = (qv_out(:,:,:,t)- qv_in(:,:,:,t))/dt(t)
    !      dqc_dt(:,:,:,t) = (qc_out(:,:,:,t)- qc_in(:,:,:,t))/dt(t)
    !      dqr_dt(:,:,:,t) = (qr_out(:,:,:,t)- qr_in(:,:,:,t))/dt(t)
    !      dqs_dt(:,:,:,t) = (qs_out(:,:,:,t)- qs_in(:,:,:,t))/dt(t)
    !    end do
    !  end associate

    !  call_assert(.not. any(ieee_is_nan(dpt_dt)))
    !  call_assert(.not. any(ieee_is_nan(dqv_dt)))
    !  call_assert(.not. any(ieee_is_nan(dqc_dt)))
    !  call_assert(.not. any(ieee_is_nan(dqr_dt)))
    !  call_assert(.not. any(ieee_is_nan(dqs_dt)))

    !  print *,"Calculating output tensor histograms."
    !  histograms = [ &
    !     histogram_t(dpt_dt, "dptdt", num_bins, raw) &
    !    ,histogram_t(dqv_dt, "dqvdt", num_bins, raw) &
    !    ,histogram_t(dqc_dt, "dqcdt", num_bins, raw) &
    !    ,histogram_t(dqr_dt, "dqrdt", num_bins, raw) &
    !    ,histogram_t(dqs_dt, "dqsdt", num_bins, raw) &
    !  ]
    !  call system_clock(t_histo_finish)

  end subroutine

end program tensor_statistics
