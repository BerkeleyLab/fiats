! Copyright (c) 2023-2025, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(tensor_names_m) tensor_names_s
  use assert_m
  use julienne_m, only : operator(.csv.), operator(.cat.)
  implicit none

  character(len=*), parameter :: inputs_key = "inputs"
  character(len=*), parameter :: outputs_key = "outputs"

contains

  module procedure from_components
    tensor_names%inputs_ = inputs
    tensor_names%outputs_ = outputs
  end procedure 

  module procedure equals
    call_assert(all([allocated(lhs%inputs_), allocated(rhs%inputs_), allocated(lhs%outputs_), allocated(rhs%outputs_)]))
    lhs_equals_rhs = all(lhs%inputs_ == rhs%inputs_) .and. all(lhs%outputs_ == rhs%outputs_)
  end procedure 

  module procedure from_json
    integer l
    logical tensor_names_key_found 

    tensor_names_key_found = .false.

    do l=1,size(lines)
      if (lines(l)%get_json_key() == "tensor names") then
        tensor_names_key_found = .true.
        tensor_names%inputs_  = lines(l+1)%get_json_value(string_t("inputs") , mold=[string_t("")])
        tensor_names%outputs_ = lines(l+2)%get_json_value(string_t("outputs"), mold=[string_t("")])
        return
      end if
    end do

    call_assert(tensor_names_key_found)
  end procedure

  module procedure to_json
    character(len=*), parameter :: indent = repeat(" ",ncopies=4)

    lines = [ &
      string_t(indent // '"tensor names": {'                 ) &
     ,indent // indent // '"inputs"  : [' // .csv. self%inputs_%bracket('"')  // '],'  &
     ,indent // indent // '"outputs" : [' // .csv. self%outputs_%bracket('"') // ']'  &
     ,string_t(indent // '}'                                 ) &
    ]

  end procedure

  module procedure input_names
    names = self%inputs_
  end procedure

  module procedure output_names
    names = self%outputs_
  end procedure

end submodule tensor_names_s
