module hyperparameters_m
  use julienne_string_m, only : string_t
  implicit none
  private
  public :: hyperparameters_t

  type hyperparameters_t(k)
    integer, kind :: k = kind(1.)
    integer, private:: mini_batches_ = 10
  contains
    generic :: to_json => default_real_to_json
    procedure, private :: default_real_to_json
    generic :: operator(==) => default_real_equals
    procedure, private ::      default_real_equals
  end type

  interface hyperparameters_t

    pure module function default_real_from_json(lines) result(hyperparameters)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(hyperparameters_t) hyperparameters
    end function

    pure module function default_real_from_components(mini_batches) result(hyperparameters)
      implicit none
      integer, intent(in) :: mini_batches
      type(hyperparameters_t) hyperparameters
    end function

  end interface

  interface

    pure module function default_real_to_json(self) result(lines)
      implicit none
      class(hyperparameters_t), intent(in) :: self
      type(string_t), allocatable :: lines(:)
    end function

    elemental module function default_real_equals(lhs, rhs) result(lhs_equals_rhs)
      implicit none
      class(hyperparameters_t), intent(in) :: lhs, rhs
      logical lhs_equals_rhs
    end function

  end interface

end module
