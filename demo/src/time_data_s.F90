! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(time_data_m) time_data_s
  use assert_m
  implicit none
contains

  module procedure from_string_array
     icar_output_file%file_t = file_t(lines)
  end procedure

  module procedure from_icar_output
    associate(lines => icar_output_file%lines())
      block 
        integer line
        integer, parameter :: space = 1, date_length = len("2010/10/01"), time_length = len("03:16:00")
        character(len=*), parameter :: preface = " training data dt="
        integer, parameter :: preface_end = len(preface)
        integer, parameter :: date_start = preface_end + space + 1, date_end = date_start + date_length - 1
        integer, parameter :: time_start =    date_end + space + 1, time_end = time_start + time_length - 1
        integer, parameter :: dt_start   =    time_end + space + 1

        associate(num_lines => size(lines))

          allocate(time_data%date_(num_lines))
          allocate(time_data%time_(num_lines))
          allocate(time_data%dt_(num_lines))
           
          do line = 1, num_lines
            associate(raw_line => lines(line)%string())
              call_assert(raw_line(1:len(preface))==preface)
              time_data%date_(line) = raw_line(date_start:date_end)
              time_data%time_(line) = raw_line(time_start:time_end)
              read(raw_line(dt_start:),*) time_data%dt_(line)
            end associate
          end do

        end associate
      end block
    end associate
  end procedure

  module procedure default_real_to_json
  end procedure

  module procedure default_real_dt
    dt_values = self%dt_
  end procedure

  module procedure default_real_from_json
  end procedure

end submodule time_data_s
