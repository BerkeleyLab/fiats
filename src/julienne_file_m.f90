! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module julienne_file_m
  !! A representation of a file as an object
  use julienne_string_m, only : string_t

  private
  public :: file_t

  type file_t
    private
    type(string_t), allocatable :: lines_(:)
  contains
    procedure :: lines
    procedure :: write_lines
  end type

  interface file_t

    impure elemental module function from_file_with_string_name(file_name) result(file_object)
      implicit none
      type(string_t), intent(in) :: file_name
      type(file_t) file_object
    end function

    impure elemental module function from_file_with_character_name(file_name) result(file_object)
      implicit none
      character(len=*), intent(in) :: file_name
      type(file_t) file_object
    end function

    pure module function from_string_array(lines) result(file_object)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(file_t) file_object
    end function

  end interface

  interface

    pure module function lines(self)  result(my_lines)
      implicit none
      class(file_t), intent(in) :: self
      type(string_t), allocatable :: my_lines(:)
    end function

    impure elemental module subroutine write_lines(self, file_name)
      implicit none
      class(file_t), intent(in) :: self
      type(string_t), intent(in), optional :: file_name
    end subroutine

  end interface

end module julienne_file_m
