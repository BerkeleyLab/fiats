module activation_m
  use julienne_m, only : string_t
  implicit none

  enum, bind(C)
    enumerator :: gelu=1, relu, sigmoid, step, swish
  end enum

  type activation_t
    integer :: selection_ = sigmoid
  end type
end module
