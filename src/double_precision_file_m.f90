module double_precision_file_m
  use julienne_m, only : file_t, string_t

  implicit none

  type, extends(file_t) :: double_precision_file_t
  end type

end module
