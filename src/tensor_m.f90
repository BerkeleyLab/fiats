module tensor_m
  implicit none

  type tensor_t(k)
    integer, kind :: k = kind(1.)
    real(k), allocatable, private :: values_(:)
  end type

  interface tensor_t

    pure module function construct_default_real(values) result(tensor)
      implicit none
      real, intent(in) :: values(:)
      type(tensor_t) tensor
    end function

    pure module function construct_double_precision(values) result(tensor)
      implicit none
      double precision, intent(in) :: values(:)
      type(tensor_t(double_precision)) tensor
    end function

  end interface

end module
