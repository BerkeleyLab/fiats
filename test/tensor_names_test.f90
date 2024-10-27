! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module tensor_names_test_m
  !! Test tensor_names_t object I/O and construction

  ! External dependencies
  use fiats_m, only : tensor_names_t
  use julienne_m, only : test_t, test_result_t, test_description_t, test_description_substring, string_t
#ifdef __GFORTRAN__
  use julienne_m, only : test_function_i
#endif

  ! Internal dependencies
  use tensor_names_m, only : tensor_names_t
  implicit none

  private
  public :: tensor_names_test_t

  type, extends(test_t) :: tensor_names_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A tensor_names_t object"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#ifndef __GFORTRAN__
    test_descriptions = [ & 
      test_description_t( &
        string_t("component-wise construction followed by conversion to and from JSON"), &
        write_then_read_tensor_names) &
    ]
#else
    procedure(test_function_i), pointer :: check_write_then_read_ptr
    check_write_then_read_ptr => write_then_read_tensor_names

    test_descriptions = [ &
      test_description_t( &
        string_t("component-wise construction followed by conversion to and from JSON"), &
        check_write_then_read_ptr) &
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

  function write_then_read_tensor_names() result(test_passes)
    logical test_passes

    associate( &
      from_components => tensor_names_t( &
        inputs  = [string_t("qc"), string_t("qv"), string_t("pressure")], &
        outputs = [string_t("qc"), string_t("qv")] &
    ) )
      associate(from_json => tensor_names_t(from_components%to_json()))
        test_passes = from_components == from_json 
      end associate
    end associate
  end function

end module tensor_names_test_m
