! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module hyperparameters_test_m
  !! Test hyperparameters_t object I/O and construction

  ! External dependencies
  use fiats_m, only : hyperparameters_t
  use julienne_m, only : &
     string_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t

  ! Internal dependencies
  use hyperparameters_m, only : hyperparameters_t

  implicit none

  private
  public :: hyperparameters_test_t

  type, extends(test_t) :: hyperparameters_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A hyperparameters_t object"
  end function

  function results() result(test_results)
    type(hyperparameters_test_t) hyperparameters_test
    type(test_result_t), allocatable :: test_results(:)

    test_results = hyperparameters_test%run([ & 
      test_description_t( "component-wise construction followed by conversion to and from JSON", write_then_read_hyperparameters) &
    ])
  end function

  function write_then_read_hyperparameters() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    associate(hyperparameters => hyperparameters_t(mini_batches=5, learning_rate=1., optimizer = "stochastic gradient descent"))
      associate(from_json => hyperparameters_t(hyperparameters%to_json()))
        test_diagnosis = test_diagnosis_t(hyperparameters == from_json, diagnostics_string="hyperparameters /= from_json")
      end associate
    end associate
  end function

end module hyperparameters_test_m
