program test_suite_driver
  use fiats_m, only : training_configuration_t, hyperparameters_t, network_configuration_t, tensor_names_t
  use julienne_m, only : file_t, string_t
  implicit none

  type(training_configuration_t) training_configuration 

  block
    type(training_configuration_t) from_json
    type(file_t) json_file

    json_file = file_t([ &
       string_t('{') &
      ,string_t('    "hyperparameters": {') &
      ,string_t('        "mini-batches" : 5,') &
      ,string_t('        "learning rate" : 1.00000000,') &
      ,string_t('        "optimizer" : "adam"') &
      ,string_t('    }') &
      ,string_t(',') &
      ,string_t('    "network configuration": {') &
      ,string_t('        "skip connections" : false,') &
      ,string_t('        "nodes per layer" : [2,                       72,                        2],') &
      ,string_t('        "activation function" : "sigmoid"') &
      ,string_t('    }') &
      ,string_t(',') &
      ,string_t('    "tensor names": {') &
      ,string_t('        "inputs"  : ["pressure","temperature"],') &
      ,string_t('        "outputs" : ["saturated mixing ratio"]') &
      ,string_t('    }') &
      ,string_t('}') &
    ])
    from_json = training_configuration_t(json_file)
  end block

end program
