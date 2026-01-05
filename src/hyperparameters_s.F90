submodule(hyperparameters_m) hyperparameters_s
  implicit none

  character(len=*), parameter :: mini_batches_key  = "mini-batches"

contains

  module procedure default_real_from_components
    hyperparameters%mini_batches_ = mini_batches
  end procedure 

  module procedure default_real_equals

    real, parameter :: tolerance = 1.E-08

    lhs_equals_rhs = &
      lhs%mini_batches_ == rhs%mini_batches_
  end procedure

  module procedure default_real_from_json
    integer l
    logical hyperparameters_key_found 

    hyperparameters_key_found = .false.

    do l=1,size(lines)
      if (lines(l)%get_json_key() == "hyperparameters") then
        hyperparameters_key_found = .true.
        hyperparameters%mini_batches_  = lines(l+1)%get_json_value(string_t(mini_batches_key), mold=0)
        return
      end if
    end do

  end procedure

  module procedure default_real_to_json
    character(len=*), parameter :: indent = repeat(" ",ncopies=4)
    integer, parameter :: max_width= 18
    character(len=max_width) mini_batches_string

    write(mini_batches_string,*) self%mini_batches_

    lines = [ &
      string_t(indent // '"hyperparameters": {'), &
      string_t(indent // indent // '"' // mini_batches_key  // '" : '  // trim(adjustl(mini_batches_string))  // "," ), &
      string_t(indent // '}') &
    ]
  end procedure

end submodule hyperparameters_s
