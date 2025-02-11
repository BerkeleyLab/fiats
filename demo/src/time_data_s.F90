! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(time_data_m) time_data_s
  use assert_m
  use julienne_m, only : separated_values, operator(.csv.)
  implicit none
contains

  module procedure from_string_array
     icar_output_file%file_t = file_t(lines)
  end procedure

  module procedure default_real_from_json

    character(len=:), allocatable :: line
    integer i

    !associate(lines => self%lines())

    !i = 1
    !call_assert_diagnose(adjustl(lines(i)%string())=='{'," default_real_json(time_data_s): object start", lines(i)%string())
    !call_assert(allocated(self%date_))
    !call_assert(allocated(self%time_))
    !call_assert(allocated(self%dt_))
    !call_assert(all(size(self%date_) == [size(self%time_), size(self%dt_)]))

    !i = 3
    !time_data%date_ = lines(i)%get_json_value(key="dates", mold=[character(len=1]::]))
    !i = 4
    !time_data%time_ = lines(i)%get_json_value(key="times", mold=[character(len=1]::]))
    !i = 5
    !time_data%dt_= lines(i)%get_json_value(key="dt", mold=[real::])

  end procedure

  module procedure default_real_from_strings
    time_data%date_ = date
    time_data%time_ = time
    time_data%dt_ = dt
  end procedure

  module procedure default_real_from_characters
    time_data%date_ = string_t(date)
    time_data%time_ = string_t(time)
    time_data%dt_ = dt
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
    integer, parameter :: characters_per_value=17, comma = 1
    character(len=*), parameter :: indent = repeat(" ",ncopies=12)
    character(len=:), allocatable :: csv_format, date_string, time_string, dt_string

    call_assert(allocated(self%date_))
    call_assert(allocated(self%time_))
    call_assert(allocated(self%dt_))
    call_assert(all(size(self%date_) == [size(self%time_), size(self%dt_)]))

    associate( &
       quoted_dates => self%date_%bracket('"') &
      ,quoted_times => self%time_%bracket('"') &
    )
     csv_format = separated_values(separator="','", mold=[real::])
     allocate(character(len=size(self%date_)*(len('"2010/10/01"' )+comma))::date_string)
     allocate(character(len=size(self%time_)*(len('"03:16:00"')+comma))::time_string)
     allocate(character(len=size(self%dt_)*(characters_per_value+1)-1)::dt_string)
     associate( &
        csv_dates => .csv. quoted_dates &
       ,csv_times => .csv. quoted_times &
       ,csv_dt    => .csv. string_t(self%dt_) &
     )
       write(date_string, fmt = csv_format) csv_dates%string()
       write(time_string, fmt = csv_format) csv_times%string()
       write(  dt_string, fmt = csv_format) csv_dt%string()
       file = file_t([ &
         string_t('{'), &
         string_t('"time_data" : {'), &
         string_t(indent // '  "dates": [' // trim(adjustl(date_string)) // '],'), &
         string_t(indent // '  "times": [' // trim(adjustl(time_string)) // '],'), &
         string_t(indent // '  "dt": [' // trim(adjustl(dt_string)) // ']'), &
         string_t(indent // '}'), &
         string_t('}') &
       ] )
     end associate
    end associate
  end procedure

  module procedure default_real_dt
    dt_values = self%dt_
  end procedure

end submodule time_data_s
