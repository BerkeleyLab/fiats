module tensor_m
  use kind_parameters_m, only : default_real, double_precision
  implicit none
  type tensor_t(k)
    integer, kind :: k = default_real 
    real(k), allocatable, private :: values_(:)
  end type
end module
