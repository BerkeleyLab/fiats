module julienne_file_m
  use julienne_string_m, only : string_t

  type file_t
    type(string_t), allocatable :: lines_(:)
  end type

  interface file_t

    pure module function from_lines(lines) result(file_object)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(file_t) file_object
    end function

  end interface

contains

  module procedure from_lines
    allocate(file_object%lines_, source=lines)
  end procedure

end module
