! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module julienne_string_m
  use characterizable_m, only : characterizable_t
  implicit none
  
  type, extends(characterizable_t) :: string_t
    character(len=:), allocatable :: string_
  contains
    procedure :: as_character
    generic :: string => as_character
    generic :: assignment(= ) => assign_string_t_to_character, assign_character_to_string_t
    procedure, private :: assign_character_to_string_t
    procedure, private, pass(rhs) :: assign_string_t_to_character
  end type

  interface

    pure module function as_character(self) result(raw_string)
      implicit none
      class(string_t), intent(in) :: self
      character(len=:), allocatable :: raw_string
    end function

    pure module subroutine assign_character_to_string_t(lhs, rhs)
      implicit none
      class(string_t), intent(inout) :: lhs
      character(len=*), intent(in) :: rhs
    end subroutine

    pure module subroutine assign_string_t_to_character(lhs, rhs)
      implicit none
      class(string_t), intent(in) :: rhs
      character(len=:), intent(out), allocatable :: lhs
    end subroutine

  end interface
  
end module julienne_string_m
