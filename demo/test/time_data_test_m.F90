! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module time_data_test_m
  !! Unit test for the time_data subroutine
  use julienne_m, only : &
     file_t &
    ,operator(.csv.) &
    ,string_t &
    ,test_t &
    ,test_result_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_description_substring
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : diagnosis_function_i
#endif
  use time_data_m, only : time_data_t, icar_output_file_t
  implicit none

  private
  public :: time_data_test_t

  type, extends(test_t) :: time_data_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The time_data_t type" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ & 
       test_description_t("constructing a instance from a file object in the icar output format", construct_from_icar_file) &
      ,test_description_t("writing and reading a JSON file returns the values written", write_then_read_json) &
    ]   
#else
    procedure(diagnosis_function_i), pointer :: construct_from_icar_file_ptr, write_then_read_json_ptr

    construct_from_icar_file_ptr => construct_from_icar_file
    write_then_read_json_ptr => write_then_read_json

    test_descriptions = [ & 
       test_description_t("constructing a instance from a file object in the icar output format", construct_from_icar_file_ptr) &
      ,test_description_t("writing and reading a JSON file returns the values written", write_then_read_json_ptr) &
    ]   
#endif

    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0  &
      .or. test_descriptions%contains_text(test_description_substring))
      
    test_results = test_descriptions%run()
  end function

  function construct_from_icar_file() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    real, parameter :: expected_dt = 120., tolerance = 1E-08
    real, allocatable :: dt(:)
    
    associate(time_data => time_data_t( icar_output_file_t( string_t( [ &
       "training data dt= 2010/10/01 03:16:00   120.000000               1" &
      ,"training data dt= 2010/10/01 06:36:00   120.000000               2" &
      ,"training data dt= 2010/10/01 09:56:00   120.000000               3" &
    ] ) ) ) )
      associate(dt => time_data%dt())
        test_diagnosis = test_diagnosis_t( &
           test_passed = all(abs(dt - expected_dt) < tolerance) &
          ,diagnostics_string = "expected " // string_t(expected_dt) // ", actual " // .csv. string_t(dt) &
        )
      end associate
    end associate
  end function

  function write_then_read_json() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(time_data_t) time_data
    real, parameter :: expected_dt = 120.000000, tolerance = 1.E-08

    associate(time_data => time_data_t( &
       date = string_t(["2010/10/01", "2010/10/01", "2010/10/01"]) &
      ,time = string_t([  "03:16:00",   "06:36:00",   "09:56:00"]) &
      ,dt   = [expected_dt, expected_dt, expected_dt] &
    ) )
      associate(json_file => time_data%to_json())
        associate(from_json => time_data_t(json_file))
          associate(actual_dt => from_json%dt())
            test_diagnosis = test_diagnosis_t( &
               test_passed = all(abs(actual_dt - expected_dt) < tolerance) &
              ,diagnostics_string = "expected " & // string_t(expected_dt)  // ", actual " & // string_t(actual_dt) &
            )
          end associate
        end associate
      end associate
    end associate
  end function

end module time_data_test_m
