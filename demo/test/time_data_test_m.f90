! Copyright (c) 2022-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module time_data_test_m
  !! Unit test for the time_data subroutine
  use julienne_m, only : &
     operator(.all.) &
    ,operator(.approximates.) &
    ,operator(.within.) &
    ,operator(//) &
    ,string_t &
    ,test_t &
    ,test_result_t &
    ,test_description_t &
    ,test_diagnosis_t
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
    type(time_data_test_t) time_data_test
    type(test_result_t), allocatable :: test_results(:)

    test_results = time_data_test%run([ &
       test_description_t("constructing a instance from a file object in the icar output format", construct_from_icar_file) &
      ,test_description_t("writing and reading a JSON file returns the values written", write_then_read_json) &
    ])
  end function

  type(test_diagnosis_t) function construct_from_icar_file() result(test_diagnosis)
    associate(time_data => time_data_t( icar_output_file_t( string_t( [ &
       "training data dt= 2010/10/01 03:16:00   120.000000               1" &
      ,"training data dt= 2010/10/01 06:36:00   120.000000               2" &
      ,"training data dt= 2010/10/01 09:56:00   120.000000               3" &
    ] ) ) ) )
      test_diagnosis = .all. (time_data%dt() .approximates. 120. .within. 1E-08) // "time step defined via icar_output_file_t constructor"
    end associate
  end function

  type(test_diagnosis_t) function write_then_read_json() result(test_diagnosis)
    real, parameter ::  expected_dt = 120.

    associate(time_data => time_data_t( &
       date = string_t(["2010/10/01", "2010/10/01", "2010/10/01"]) &
      ,time = string_t([  "03:16:00",   "06:36:00",   "09:56:00"]) &
      ,dt   = [expected_dt, expected_dt, expected_dt] &
    ) )
      associate(from_json => time_data_t(time_data%to_json()))
        test_diagnosis = .all. (from_json%dt() .approximates. expected_dt .within. 1E-08) // "time step read from JSON file_t object"
      end associate
    end associate
  end function

end module time_data_test_m
