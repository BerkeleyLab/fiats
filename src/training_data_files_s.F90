! Copyright (c) 2023-2025, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(training_data_files_m) training_data_files_s
  use assert_m
  use julienne_m, only : operator(.csv.)
  implicit none

  character(len=*), parameter :: training_data_files_key = "training data files"
  character(len=*), parameter :: path_key  = "path"
  character(len=*), parameter :: inputs_prefix_key = "inputs prefix" 
  character(len=*), parameter :: outputs_prefix_key = "outputs prefix"
  character(len=*), parameter :: infixes_key = "infixes" 
  character(len=*), parameter :: suffix = ".nc" 

contains

  module procedure path
     training_data_file_path = self%path_
  end procedure

  module procedure fully_qualified_time_file
    name = self%path_ // "/dt.json"
  end procedure

  module procedure to_json
    character(len=*), parameter :: indent = repeat(" ",ncopies=4)

    file = file_t([ &
       string_t(indent // '"' //training_data_files_key //  '": {'                                 ) &
      ,string_t(indent // indent // '"' // path_key           // '" : "' // self%path_           // '",') &
      ,string_t(indent // indent // '"' // inputs_prefix_key  // '" : "' // self%inputs_prefix_  // '",') &
      ,string_t(indent // indent // '"' // outputs_prefix_key // '" : "' // self%outputs_prefix_ // '",') &
      ,         indent // indent // '"infixes"  : [' // .csv. self%infixes_%bracket('"')  // ']'          &
      ,string_t(indent // '}'                                                                           ) &
    ])

  end procedure

  module procedure from_json
    integer l
    logical training_data_files_key_found

    training_data_files_key_found = .false.

    associate(lines => file%lines())
      do l=1,size(lines)
        if (lines(l)%get_json_key() == training_data_files_key) then
          training_data_files_key_found = .true.
          training_data_files%path_ = lines(l+1)%get_json_value(string_t(path_key), mold=string_t(""))
          training_data_files%inputs_prefix_ = lines(l+2)%get_json_value(string_t(inputs_prefix_key), mold=string_t(""))
          training_data_files%outputs_prefix_ = lines(l+3)%get_json_value(string_t(outputs_prefix_key), mold=string_t(""))
          training_data_files%infixes_ = lines(l+4)%get_json_value(string_t(infixes_key), mold=[string_t("")])
          return
        end if
      end do
    end associate

    call_assert(training_data_files_key_found)
  end procedure

  module procedure from_components
    training_data_files%path_ = path
    training_data_files%inputs_prefix_ = inputs_prefix
    training_data_files%outputs_prefix_ = outputs_prefix
    training_data_files%infixes_ = infixes
  end procedure

  module procedure equals
    lhs_eq_rhs = lhs%path_           == rhs%path_ &
           .and. lhs%inputs_prefix_  == rhs%inputs_prefix_ &
           .and. lhs%outputs_prefix_ == rhs%outputs_prefix_ &
           .and. all(lhs%infixes_    == rhs%infixes_)
  end procedure

  module procedure fully_qualified_inputs_files
    integer i
    names = [(self%path_ // "/" // self%inputs_prefix_ //self%infixes_(i) // suffix, i=1, size(self%infixes_))]
  end procedure

  module procedure fully_qualified_outputs_files
    integer i
    names = [(self%path_ // "/" // self%outputs_prefix_ //self%infixes_(i) // suffix, i=1, size(self%infixes_))]
  end procedure

end submodule training_data_files_s
