module tensor_map_m
  use kind_parameters_m, only : default_real
  implicit none
  
  type tensor_map_t(k)
    integer, kind :: k = default_real 
    character(len=:),      allocatable, private :: layer_
    real(k), dimension(:), allocatable, private :: intercept_, slope_
  end type

end module
