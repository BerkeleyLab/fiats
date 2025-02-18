module tensor_m
  implicit none
  type tensor_t(k)
    integer, kind :: k = kind(1.)
    real(k), allocatable, private :: values_(:)
  end type
end module
