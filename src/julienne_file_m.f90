module julienne_file_m
  use julienne_string_m, only : string_t

  type file_t
    type(string_t), allocatable :: lines_(:)
  end type

  interface file_t

    module function from_file_with_string_name(file_name) result(file_object)
      implicit none
      type(string_t), intent(in) :: file_name
      type(file_t) file_object
    end function

    module function from_file_with_character_name(file_name) result(file_object)
      implicit none
      character(len=*), intent(in) :: file_name
      type(file_t) file_object
    end function

    pure module function from_lines(lines) result(file_object)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(file_t) file_object
    end function

  end interface

end module
