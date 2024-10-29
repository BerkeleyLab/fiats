! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(training_configuration_m) training_configuration_s
  use assert_m, only : assert
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

  module procedure double_precision_from_components

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
#if defined __INTEL_COMPILER || _CRAYFTN
    type(string_t), allocatable :: lines(:)
#endif

    training_configuration%file_t = file_object

#if defined __INTEL_COMPILER || _CRAYFTN
    lines = training_configuration%file_t%lines()
#else
    associate(lines => training_configuration%file_t%lines())
#endif

      training_configuration%hyperparameters_ = hyperparameters_t(lines)
      training_configuration%network_configuration_= network_configuration_t(lines)
      training_configuration%tensor_names_ = tensor_names_t(lines)

#if ! defined __INTEL_COMPILER || _CRAYFTN
    end associate
#endif

  end procedure

  module procedure double_precision_from_file
#if defined __INTEL_COMPILER || _CRAYFTN
    type(double_precision_string_t), allocatable :: lines(:)
#endif

    training_configuration%double_precision_file_t = file_object

#if defined __INTEL_COMPILER || _CRAYFTN
    lines = training_configuration%double_precision_file_t%double_precision_lines()
#else
    associate(lines => training_configuration%double_precision_file_t%double_precision_lines())
#endif

      training_configuration%hyperparameters_ = hyperparameters_t(lines)
      training_configuration%network_configuration_= network_configuration_t(lines)
      training_configuration%tensor_names_ = tensor_names_t(lines)

#if ! defined __INTEL_COMPILER || _CRAYFTN
    end associate
#endif
  end procedure

  module procedure default_real_to_json
    json_lines = self%lines()
  end procedure

  module procedure double_precision_to_json
    json_lines = self%lines()
  end procedure

  module procedure default_real_equals
    lhs_eq_rhs = &
      lhs%hyperparameters_ == rhs%hyperparameters_ .and. &
      lhs%network_configuration_ == rhs%network_configuration_ .and. &
      lhs%tensor_names_ == rhs%tensor_names_
  end procedure

  module procedure double_precision_equals
    lhs_eq_rhs = &
      lhs%hyperparameters_ == rhs%hyperparameters_ .and. &
      lhs%network_configuration_ == rhs%network_configuration_ .and. &
      lhs%tensor_names_ == rhs%tensor_names_
  end procedure

  module procedure default_real_mini_batches
    num_mini_batches = self%hyperparameters_%mini_batches()
  end procedure

  module procedure double_precision_mini_batches
    num_mini_batches = self%hyperparameters_%mini_batches()
  end procedure

  module procedure default_real_optimizer_name
    identifier = self%hyperparameters_%optimizer_name()
  end procedure

  module procedure double_precision_optimizer_name
    identifier = self%hyperparameters_%optimizer_name()
  end procedure

  module procedure default_real_learning_rate
    rate = self%hyperparameters_%learning_rate()
  end procedure

  module procedure double_precision_learning_rate
    rate = self%hyperparameters_%learning_rate()
  end procedure

  module procedure default_real_nodes_per_layer
    nodes = self%network_configuration_%nodes_per_layer()
  end procedure

  module procedure double_precision_nodes_per_layer
    nodes = self%network_configuration_%nodes_per_layer()
  end procedure

  module procedure default_real_skip_connections
    using_skip = self%network_configuration_%skip_connections()
  end procedure

  module procedure double_precision_skip_connections
    using_skip = self%network_configuration_%skip_connections()
  end procedure

  module procedure default_real_activation
#if defined __INTEL_COMPILER || _CRAYFTN
    type(string_t) :: activation_name
    activation_name = self%network_configuration_%activation_name()
#else
    associate(activation_name => self%network_configuration_%activation_name())
#endif
      select case(activation_name%string())
        case ("gelu")
          activation = activation_t(gelu)
        case ("relu")
          activation = activation_t(relu)
        case ("sigmoid")
          activation = activation_t(sigmoid)
        case ("swish")
          activation = activation_t(swish)
        case default
          error stop 'activation_factory_s(factory): unrecognized activation name "' // activation_name%string() // '"' 
      end select
#if ! (defined __INTEL_COMPILER || _CRAYFTN)
    end associate
#endif
  end procedure

  module procedure double_precision_activation
#if defined __INTEL_COMPILER || _CRAYFTN
    type(string_t) :: activation_name
    activation_name = self%network_configuration_%activation_name()
#else
    associate(activation_name => self%network_configuration_%activation_name())
#endif
      select case(activation_name%string())
        case ("gelu")
          activation = activation_t(gelu)
        case ("relu")
          activation = activation_t(relu)
        case ("sigmoid")
          activation = activation_t(sigmoid)
        case ("swish")
          activation = activation_t(swish)
        case default
          error stop 'activation_factory_s(factory): unrecognized activation name "' // activation_name%string() // '"' 
      end select
#if ! (defined __INTEL_COMPILER || _CRAYFTN)
    end associate
#endif
  end procedure

  module procedure default_real_input_names
    input_names = self%tensor_names_%input_names()
  end procedure

  module procedure double_precision_input_names
    input_names = self%tensor_names_%input_names()
  end procedure

  module procedure default_real_output_names
    output_names = self%tensor_names_%output_names()
  end procedure

  module procedure double_precision_output_names
    output_names = self%tensor_names_%output_names()
  end procedure

end submodule training_configuration_s
