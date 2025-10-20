! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module training_data_files_test_m
  !! Test training_data_files_t object I/O and construction

  ! External dependencies
  use fiats_m, only : training_data_files_t
  use julienne_m, only : &
     string_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t

  ! Internal dependencies
  use fiats_m, only : training_data_files_t

  implicit none

  private
  public :: training_data_files_test_t

  type, extends(test_t) :: training_data_files_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A training_data_files_t object"
  end function

  function results() result(test_results)
    type(training_data_files_test_t) training_data_files_test
    type(test_result_t), allocatable :: test_results(:)

    test_results = training_data_files_test%run([ & 
      test_description_t( string_t("round-trip to/from JSON yielding equivalent objects"), check_json_round_trip) &
    ])
  end function

  function check_json_round_trip() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    associate(constructed_file => training_data_files_t( &
       path = "dates-20101001-2011076" &
      ,inputs_prefix  = "training_input-image-" &
      ,outputs_prefix = "training_output-image-" &
      ,infixes = string_t(["000001", "000002", "000003", "000004", "000005", "000006", "000007", "000008", "000009", "000010"]) &
    ))
      associate(from_json => training_data_files_t(constructed_file%to_json()))
        test_diagnosis = test_diagnosis_t( &
           test_passed = constructed_file == from_json &
          ,diagnostics_string = "constructed_file /= from_json" &
      )
      end associate
    end associate
  end function

end module training_data_files_test_m
