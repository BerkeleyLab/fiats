! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module tensor_names_m
  use julienne_string_m, only : string_t
  implicit none

  private
  public :: tensor_names_t

  type tensor_names_t
    private
    type(string_t), allocatable :: inputs_(:), outputs_(:)
  contains
    procedure :: to_json
    procedure :: equals
    procedure :: input_names
    procedure :: output_names
    generic :: operator(==) => equals
  end type

  interface tensor_names_t

    pure module function from_json(lines) result(tensor_names)
      implicit none
      class(string_t), intent(in) :: lines(:)
      type(tensor_names_t) tensor_names
    end function

    pure module function from_components(inputs, outputs) result(tensor_names)
      implicit none
      type(string_t), intent(in) :: inputs(:), outputs(:)
      type(tensor_names_t) tensor_names
    end function

  end interface

  interface

    pure module function to_json(self) result(lines)
      implicit none
      class(tensor_names_t), intent(in) :: self
      type(string_t), allocatable :: lines(:)
    end function

    elemental module function equals(lhs, rhs) result(lhs_equals_rhs)
      implicit none
      class(tensor_names_t), intent(in) :: lhs, rhs
      logical lhs_equals_rhs
    end function

    pure module function input_names(self) result(names)
      implicit none
      class(tensor_names_t), intent(in) :: self
      type(string_t), allocatable :: names(:)
    end function

    pure module function output_names(self) result(names)
      implicit none
      class(tensor_names_t), intent(in) :: self
      type(string_t), allocatable :: names(:)
    end function

  end interface

end module
