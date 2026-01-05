module hyperparameters_m
  use julienne_string_m, only : string_t
  implicit none
  private
  public :: hyperparameters_t

  type hyperparameters_t(k)
    integer, kind :: k = kind(1.)
    integer, private:: mini_batches_ = 10
  contains
    generic :: to_json => default_real_to_json
    procedure, private :: default_real_to_json
    generic :: operator(==) => default_real_equals
    procedure, private ::      default_real_equals
  end type

  interface hyperparameters_t

    pure module function default_real_from_json(lines) result(hyperparameters)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(hyperparameters_t) hyperparameters
    end function

    pure module function default_real_from_components(mini_batches) result(hyperparameters)
      implicit none
      integer, intent(in) :: mini_batches
      type(hyperparameters_t) hyperparameters
    end function

  end interface

  interface

    pure module function default_real_to_json(self) result(lines)
      implicit none
      class(hyperparameters_t), intent(in) :: self
      type(string_t), allocatable :: lines(:)
    end function

    elemental module function default_real_equals(lhs, rhs) result(lhs_equals_rhs)
      implicit none
      class(hyperparameters_t), intent(in) :: lhs, rhs
      logical lhs_equals_rhs
    end function

  end interface


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

end module
