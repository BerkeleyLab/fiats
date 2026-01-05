module julienne_file_m
  use julienne_string_m, only : string_t

  type file_t
    type(string_t), allocatable :: lines_(:)
  end type

  interface file_t
    module procedure from_lines
  end interface

contains

  type(file_t) pure function from_lines(lines)
    type(string_t), intent(in) :: lines(:)
    allocate(from_lines%lines_, source=lines) ! switching this to an assignment (from_lines%lines_ = lines) prevents the seg fault
  end function

end module
