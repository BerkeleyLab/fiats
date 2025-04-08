! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module training_data_file_names_test_m
  !! Test training_data_file_names_t object I/O and construction

  ! External dependencies
  use fiats_m, only : training_data_file_names_t
  use julienne_m, only : test_t, test_result_t, test_description_t, test_description_substring, string_t, file_t
#if ! defined(HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY)
  use julienne_m, only : test_function_i
#endif

  ! Internal dependencies
  use training_data_file_names_m, only : training_data_file_names_t

  implicit none

  private
  public :: training_data_file_names_test_t

  type, extends(test_t) :: training_data_file_names_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A training_data_file_names_t object"
  end function

  function results() result(test_results)
    type(test_description_t), allocatable :: test_descriptions(:)
    type(test_result_t), allocatable :: test_results(:)

#if defined(HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY)
    test_descriptions = [ & 
      test_description_t( string_t("round-trip to/from JSON yielding equivalent objects"), check_json_round_trip) &
    ]
#else
    procedure(test_function_i), pointer :: check_json_round_trip_ptr
    check_json_round_trip_ptr => check_json_round_trip

    test_descriptions = [ &
      test_description_t(string_t("round-trip to/from JSON yielding equivalent objects"), check_json_round_trip_ptr) &
    ]
#endif
    associate( &
      substring_in_subject => index(subject(), test_description_substring) /= 0, &
      substring_in_description => test_descriptions%contains_text(string_t(test_description_substring)) &
    )
      test_descriptions = pack(test_descriptions, substring_in_subject .or. substring_in_description)
    end associate
    test_results = test_descriptions%run()
  end function

  function check_json_round_trip() result(test_passes)
    logical test_passes
    associate(training_data_file_names => training_data_file_names_t( &
       path = "dates-20101001-2011076" &
      ,inputs_prefix  = "training_input-image-" &
      ,outputs_prefix = "training_output-image-" &
      ,infixes = string_t(["000001", "000002", "000003", "000004", "000005", "000006", "000007", "000008", "000009", "000010"]) &
    ))
      associate(from_json => training_data_file_names_t(training_data_file_names%to_json()))
        test_passes = training_data_file_names == from_json
      end associate
    end associate
  end function

end module training_data_file_names_test_m
