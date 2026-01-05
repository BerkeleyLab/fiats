program test_suite_driver
  use training_configuration_m, only : training_configuration_t
  use hyperparameters_m, only : hyperparameters_t
  use julienne_file_m, only : file_t
  implicit none

  type(training_configuration_t) training_configuration 

  training_configuration = training_configuration_t(hyperparameters_t(mini_batches=5, learning_rate=1., optimizer = "adam"))

  block
    type(training_configuration_t) from_json
    from_json = training_configuration_t(file_t(training_configuration%file_t%lines_))
  end block

end program
