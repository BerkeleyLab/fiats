module training_configuration_m
  use activation_m, only : activation_t
  use julienne_string_m, only : string_t
  use julienne_file_m, only : file_t
  use hyperparameters_m, only : hyperparameters_t
  use double_precision_file_m, only : double_precision_file_t
  implicit none

  private
  public :: training_configuration_t

  type, extends(double_precision_file_t) :: training_configuration_t(m)
    integer, kind :: m = kind(1.)
    type(hyperparameters_t(m)),    private :: hyperparameters_
  contains
    generic :: operator(==)     => default_real_equals           
    procedure, private          :: default_real_equals           
    generic :: to_json          => default_real_to_json          
    procedure, private          :: default_real_to_json          
  end type
  interface training_configuration_t

    pure module function default_real_from_components(hyperparameters) &
      result(training_configuration)
      implicit none
      type(hyperparameters_t), intent(in) :: hyperparameters
      type(training_configuration_t) training_configuration
    end function

    module function default_real_from_file(file_object) result(training_configuration)
      implicit none
      type(file_t), intent(in) :: file_object
      type(training_configuration_t) training_configuration
    end function

  end interface

  interface

    pure module function default_real_to_json(self) result(json_lines)
      implicit none
      class(training_configuration_t), intent(in) :: self
      type(string_t), allocatable :: json_lines(:)
    end function

    elemental module function default_real_equals(lhs, rhs) result(lhs_eq_rhs)
      implicit none
      class(training_configuration_t), intent(in) :: lhs, rhs
      logical lhs_eq_rhs
    end function

  end interface

end module
