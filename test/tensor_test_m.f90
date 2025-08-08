! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module tensor_test_m
  !! Define inference tests and procedures required for reporting results

  ! External dependencies
  use kind_parameters_m, only : double_precision 
  use julienne_m, only : &
     operator(.all.) &
    ,operator(.also.) &
    ,operator(.approximates.) &
    ,operator(.equalsExpected.) &
    ,operator(.within.) &
    ,string_t &
    ,test_description_substring &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t

  ! Internal dependencies
  use fiats_m, only : tensor_t

  implicit none

  private
  public :: tensor_test_t

  type, extends(test_t) :: tensor_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "An tensor_t object" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

    test_descriptions = [ &
      test_description_t("double-precision construction and value extraction", double_precision_construction) &
    ]
    associate( &
      substring_in_subject => index(subject(), test_description_substring) /= 0, &
      substring_in_description => test_descriptions%contains_text(string_t(test_description_substring)) &
    )
      test_descriptions = pack(test_descriptions, substring_in_subject .or. substring_in_description)
    end associate
    test_results = test_descriptions%run()
  end function

  function double_precision_construction() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(tensor_t(double_precision)) tensor
    double precision, parameter :: tolerance = 1.0D-12
    double precision, parameter :: values(*) = [1.000000000001D-12]

    tensor = tensor_t(values) ! this will fail to compile if no double_precision constructor exists and the 
    associate(tensor_values => tensor%values())
      test_diagnosis = &
        (kind(tensor_values) .equalsExpected. double_precision) &
        .also. (.all. (tensor_values .approximates. values .within. tolerance))
    end associate
  end function

end module tensor_test_m
