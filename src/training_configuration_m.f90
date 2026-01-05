module training_configuration_m
  use julienne_file_m, only : file_t
  use julienne_string_m, only : string_t
  use hyperparameters_m, only : hyperparameters_t
  implicit none

  type, extends(file_t) :: training_configuration_t(m)
    integer, kind :: m = kind(1.)
    type(hyperparameters_t(m)) hyperparameters_
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


contains

  module procedure default_real_from_components

    training_configuration%hyperparameters_ = hyperparameters

    training_configuration%file_t = file_t([ &
      string_t("{"), &
      training_configuration%hyperparameters_%to_json(), &
      string_t("}") &
    ])

  end procedure

  module procedure default_real_from_file
    training_configuration%file_t = file_object
    training_configuration%hyperparameters_ = hyperparameters_t(training_configuration%file_t%lines_)
  end procedure


end module
