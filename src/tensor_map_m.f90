module tensor_map_m
  implicit none

  type tensor_map_t(k)
    integer, kind :: k = kind(1.)
    character(len=:),      allocatable, private :: layer_
    real(k), dimension(:), allocatable, private :: intercept_, slope_
  end type

end module
