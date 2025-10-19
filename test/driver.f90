! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

program test_suite_driver
  use julienne_m, only : test_fixture_t, test_harness_t
  use asymmetric_network_test_m, only : asymmetric_network_test_t
  use hyperparameters_test_m, only : hyperparameters_test_t
  use metadata_test_m, only : metadata_test_t
  use network_configuration_test_m, only : network_configuration_test_t
  use neural_network_test_m, only : neural_network_test_t
  use tensor_map_test_m, only : tensor_map_test_t
  use tensor_names_test_m, only : tensor_names_test_t
  use tensor_test_m, only : tensor_test_t
  use trainable_network_test_m, only : trainable_network_test_t
  use training_configuration_test_m, only : training_configuration_test_t
  use training_data_files_test_m, only : training_data_files_test_t
  implicit none

  associate(test_harness => test_harness_t([ &
     test_fixture_t(asymmetric_network_test_t()) &
    ,test_fixture_t(hyperparameters_test_t()) &
    ,test_fixture_t(metadata_test_t()) &
    ,test_fixture_t(network_configuration_test_t()) &
    ,test_fixture_t(neural_network_test_t()) &
    ,test_fixture_t(tensor_map_test_t()) &
    ,test_fixture_t(tensor_names_test_t()) &
    ,test_fixture_t(tensor_test_t()) &
    ,test_fixture_t(trainable_network_test_t()) &
    ,test_fixture_t(training_configuration_test_t()) &
    ,test_fixture_t(training_data_files_test_t()) &
  ]))
    call test_harness%report_results
  end associate
end program test_suite_driver
