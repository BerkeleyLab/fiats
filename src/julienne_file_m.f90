module julienne_file_m
  use julienne_string_m
  type file_t
    type(string_t), allocatable :: lines_(:)
  end type
end module
