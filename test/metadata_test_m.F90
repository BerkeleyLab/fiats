! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module metadata_test_m
  !! Test metadata_t object I/O and construction

  ! External dependencies
  use fiats_m, only : metadata_t
  use julienne_m, only : &
     string_t &
    ,test_diagnosis_t &
    ,test_description_t &
    ,test_result_t &
    ,test_t

  ! Internal dependencies
  use metadata_m, only : metadata_t

  implicit none

  private
  public :: metadata_test_t

  type, extends(test_t) :: metadata_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A metadata_t object"
  end function

  function results() result(test_results)
    type(metadata_test_t) metadata_test
    type(test_result_t), allocatable :: test_results(:)
    test_results = metadata_test%run([ & 
      test_description_t( "component-wise construction followed by conversion to and from JSON", write_then_read_metadata) &
    ])
  end function

  function write_then_read_metadata() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    associate(metadata => &
      metadata_t( &
        modelName = string_t("Metadata Unit Test"), &
        modelAuthor = string_t("Julienne"), &
        compilationDate = string_t("2024-06-27"), &
        activationFunction = string_t("sigmoid"), &
        usingSkipConnections = string_t("false") &
      ) &
    )
      associate(from_json => metadata_t(metadata%to_json()))
        test_diagnosis = test_diagnosis_t(metadata == from_json, "metadata /= from_json")
      end associate
    end associate
  end function

end module metadata_test_m
