submodule(training_configuration_m) training_configuration_s
  implicit none

  character(len=*), parameter :: header="{", footer="}", separator = ","

contains

  module procedure default_real_from_components

    training_configuration%hyperparameters_ = hyperparameters

    training_configuration%file_t = file_t([ &
      string_t(header), &
      training_configuration%hyperparameters_%to_json(), &
      string_t(footer) &
    ])

  end procedure

  module procedure default_real_from_file
    training_configuration%file_t = file_object
    training_configuration%hyperparameters_ = hyperparameters_t(training_configuration%file_t%lines_)
  end procedure

  module procedure to_json
    json_lines = self%lines_
  end procedure

end submodule training_configuration_s
