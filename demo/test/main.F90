! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

program main
  use netCDF_file_test_m, only : netCDF_file_test_t
  use time_data_test_m, only : time_data_test_t
  use iso_fortran_env, only : int64, real64
  use julienne_m, only : command_line_t
  implicit none

  integer(int64) t_start, t_finish, clock_rate
  type(netCDF_file_test_t) netCDF_file_test
  type(time_data_test_t) time_data_test
  integer :: passes=0, tests=0

  print_usage_if_help_requested: &
  block
    type(command_line_t) command_line
    character(len=*), parameter :: usage = &
      new_line('') // new_line('') // &
      'Usage: fpm test -- [--help] | [--contains <substring>]' // &
      new_line('') // new_line('') // &
      'where square brackets ([]) denote optional arguments, a pipe (|) separates alternative arguments,' // new_line('') // &
      'angular brackets (<>) denote a user-provided value, and passing a substring limits execution to' // new_line('') // &
      'the tests with test subjects or test descriptions containing the user-specified substring.' // new_line('')
    if (command_line%argument_present([character(len=len("--help"))::"--help","-h"])) stop usage
  end block print_usage_if_help_requested

  call system_clock(t_start, clock_rate)
  call netCDF_file_test%report(passes, tests)
  call time_data_test%report(passes, tests)
  call system_clock(t_finish)

  print *
  print *,"Test suite execution time: ", real(t_finish - t_start, real64)/real(clock_rate, real64)
  print *
  print '(*(a,:,g0))',"_________ In total, ",passes," of ",tests, " tests pass. _________"
#if defined(MULTI_IMAGE_SUPPORT)
  sync all
#endif
  print *
  if (passes/=tests) error stop "-------- One or more tests failed. See the above report. ---------"
end program
