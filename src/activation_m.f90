module activation_m
  use iso_c_binding, only :  c_int
  implicit none
  type activation_t
    integer(c_int) :: selection_
  end type
end module
