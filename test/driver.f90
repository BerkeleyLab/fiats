program test_suite_driver
  use fiats_m, only : training_configuration_t, hyperparameters_t, network_configuration_t
  use julienne_m, only : file_t, string_t
  implicit none

  type(training_configuration_t) training_configuration 

  training_configuration = training_configuration_t( &
    hyperparameters_t(mini_batches=5, learning_rate=1., optimizer = "adam") &
   ,network_configuration_t(skip_connections=.false., nodes_per_layer=[2,72,2], activation_name="sigmoid") &
  )
  block
    type(training_configuration_t) from_json
    type(file_t) json_file
    json_file = file_t(training_configuration%to_json())
    call json_file%write_lines()
    from_json = training_configuration_t(json_file)
  end block

end program
