! Copyright (c) 2023-2025, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(training_configuration_m) training_configuration_s
  use double_precision_string_m, only : double_precision_string_t
  use activation_m, only : activation_t, gelu, relu, sigmoid, swish
  implicit none

  character(len=*), parameter :: header="{", footer="}", separator = ","

contains

  module procedure default_real_from_components

    training_configuration%hyperparameters_ = hyperparameters
    training_configuration%network_configuration_ = network_configuration
    training_configuration%tensor_names_ = tensor_names

    training_configuration%file_t = file_t([ &
      string_t(header), &
      training_configuration%hyperparameters_%to_json(), &
      string_t(separator), &
      training_configuration%network_configuration_%to_json(), &
      string_t(separator), &
      training_configuration%tensor_names_%to_json(), &
      string_t(footer) &
    ])

  end procedure

  module procedure default_real_from_file
    training_configuration%file_t = file_object

    associate(lines => training_configuration%file_t%lines())
      training_configuration%hyperparameters_ = hyperparameters_t(lines)
      training_configuration%network_configuration_= network_configuration_t(lines)
      training_configuration%tensor_names_ = tensor_names_t(lines)
    end associate
  end procedure

  module procedure default_real_to_json
    json_lines = self%lines()
  end procedure

  module procedure default_real_equals
    lhs_eq_rhs = &
      lhs%hyperparameters_ == rhs%hyperparameters_ .and. &
      lhs%network_configuration_ == rhs%network_configuration_ .and. &
      lhs%tensor_names_ == rhs%tensor_names_
  end procedure

end submodule training_configuration_s
