program test_suite_driver
  use fiats_m, only : training_configuration_t, hyperparameters_t, network_configuration_t, tensor_names_t
  use julienne_m, only : &
     file_t &
    ,string_t
  implicit none

    block
      type(training_configuration_t) from_json, training_configuration 
      training_configuration = training_configuration_t( &
        hyperparameters_t(mini_batches=5, learning_rate=1., optimizer = "adam") &
       ,network_configuration_t(skip_connections=.false., nodes_per_layer=[2,72,2], activation_name="sigmoid") &
       ,tensor_names_t(inputs=[string_t("pressure"), string_t("temperature")], outputs=[string_t("saturated mixing ratio")]) &
      )
      block
        type(training_configuration_t) from_json
        from_json = training_configuration_t(file_t(training_configuration%to_json()))
        stop "--------> made it"
      end block
    end block

end program test_suite_driver
