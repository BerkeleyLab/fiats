module julienne_string_m
  implicit none
  
  type string_t
    character(len=:), allocatable :: string_
  contains
    generic :: assignment(= ) => assign_string_t_to_character, assign_character_to_string_t
    procedure, private :: assign_character_to_string_t
    procedure, private, pass(rhs) :: assign_string_t_to_character
  end type

  type file_t
    type(string_t), allocatable :: lines_(:)
  end type

  interface
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

end module 
