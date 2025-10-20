! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module network_configuration_test_m
  !! Test network_configuration_t object I/O and construction

  ! External dependencies
  use fiats_m, only : network_configuration_t
  use julienne_m, only : &
    string_t &
   ,test_description_t &
   ,test_diagnosis_t &
   ,test_result_t &
   ,test_t

  ! Internal dependencies
  use network_configuration_m, only : network_configuration_t
  implicit none

  private
  public :: network_configuration_test_t

  type, extends(test_t) :: network_configuration_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A network_configuration_t object"
  end function

  function results() result(test_results)
    type(network_configuration_test_t) network_configuration_test
    type(test_result_t), allocatable :: test_results(:)

    test_results = network_configuration_test%run([ & 
      test_description_t("component-wise construction and then conversion to and from JSON", write_then_read_network_configuration) &
    ])
  end function

  function write_then_read_network_configuration() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    associate(constructed_from_components=> &
      network_configuration_t(skip_connections=.false., nodes_per_layer=[2,72,2], activation_name="sigmoid"))
      associate(constructed_from_json => network_configuration_t(constructed_from_components%to_json()))
        test_diagnosis = test_diagnosis_t( &
           test_passed =         constructed_from_components == constructed_from_json  &
          ,diagnostics_string = "constructed_from_components /= constructed_from_json" &
        )
      end associate
    end associate
  end function

end module network_configuration_test_m
