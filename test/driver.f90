program test_suite_driver
  use julienne_m, only : test_fixture_t, test_harness_t
  use training_configuration_test_m, only : training_configuration_test_t
  implicit none

  associate(test_harness => test_harness_t([ &
     test_fixture_t(training_configuration_test_t()) &
  ]))
    call test_harness%report_results
  end associate
end program test_suite_driver
