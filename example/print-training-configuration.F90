! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program print_training_configuration
  !! Demonstrate how to construct and print a training_configuration_t object
  use fiats_m, only : training_configuration_t, hyperparameters_t, network_configuration_t, tensor_names_t
  use julienne_m, only : file_t, string_t
  implicit none
#ifndef _CRAYFTN
  associate(training_configuration => training_configuration_t( &
     hyperparameters_t(mini_batches=10, learning_rate=1.5, optimizer = "adam")  &
    ,network_configuration_t(skip_connections=.false., nodes_per_layer=[2,72,2], activation_name="sigmoid") &
    ,tensor_names_t(inputs = [string_t("pressure"), string_t("temperature")], outputs = ([string_t("saturated mixing ratio")])) &
  ))
    associate(json_file => file_t(training_configuration%to_json()))
      call json_file%write_lines()
    end associate
  end associate
#else
  block
    type(training_configuration_t) :: training_configuration
    type(file_t) :: json_file
    training_configuration = training_configuration_t( &
       hyperparameters_t(mini_batches=10, learning_rate=1.5, optimizer = "adam") &
      ,network_configuration_t(skip_connections=.false., nodes_per_layer=[2,72,2], activation_name="sigmoid") &
      ,tensorm_names_t(inputs=[string_t("pressure"), string_t("temperature")], outputs([string_t("saturated mixing ratio")])) &
    )
    json_file = file_t(training_configuration%to_json())
    call json_file%write_lines()
  end block
#endif
end program
