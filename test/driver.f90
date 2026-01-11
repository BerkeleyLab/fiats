program test_suite_driver
  use training_configuration_m, only : training_configuration_t
  use hyperparameters_m, only : hyperparameters_t
  use julienne_file_m, only : file_t
  use julienne_string_m, only : string_t
  implicit none

  type(training_configuration_t) training_configuration 

  training_configuration = training_configuration_t( hyperparameters_t(mini_batches=5, learning_rate=1., optimizer = "adam"))
  ! Removing the above assignment eliminates the segmentation fault even though the segmentation fault occurs
  ! when executing the assignment below, which does not reference the object define above.

  print *,new_line(''), "Heading down the rabbit hole..." ! print to demonstrate that execution continues past the above line

  block
    type(training_configuration_t) from_json
    from_json = training_configuration_t(file_t( &
       [ string_t('{') &
        ,string_t('    "hyperparameters": {') &
        ,string_t('        "mini-batches" : 5,') &
        ,string_t('        "learning rate" : 1.00000000,') &
        ,string_t('        "optimizer" : "adam"') &
        ,string_t('    }') &
        ,string_t('}') &
       ] &
    ))
  end block

end program
