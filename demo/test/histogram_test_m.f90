! Copyright (c) 2022-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module histogram_test_m
  !! Unit test for the histogram subroutine
  use julienne_m, only : &
     operator(.all.) &
    ,operator(.approximates.) &
    ,operator(.equalsExpected.) &
    ,operator(.within.) &
    ,operator(//) &
    ,string_t &
    ,test_t &
    ,test_result_t &
    ,test_description_t &
    ,test_diagnosis_t
  use histogram_m, only : histogram_t, to_file
  implicit none

  private
  public :: histogram_test_t

  type, extends(test_t) :: histogram_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The histogram_t type" 
  end function

  function results() result(test_results)
    type(histogram_test_t) histogram_test
    type(test_result_t), allocatable :: test_results(:)

    test_results = histogram_test%run([ &
       test_description_t("constructing a uniform distribution from uniformly distributed points", check_uniform_distribution) &
    ])
  end function

  type(test_diagnosis_t) function check_uniform_distribution() result(test_diagnosis)
      
    real, target :: v(5,4,3,2)
    real, pointer :: v_1D(:)
    integer, parameter :: num_points = size(v), num_bins = 7, remainder = mod(num_points,num_bins)
    real, parameter :: v_min = -10., v_max = 10., dv = (v_max - v_min)/num_bins
    integer b, p

    v_1D(1:num_points) => v
    v_1D = [( [( v_min + dv/2 + (b-1)*dv, p = 1, num_points/num_bins + merge(0, 1, b > remainder))], b = 1, num_bins)]
    v_1D(1) = v_min
    v_1D(num_points) = v_max

    associate(histogram => histogram_t("uniform", v, minval(v), maxval(v), num_bins=num_bins))
      associate(block_distribution => num_points/num_bins + [(merge(0, 1, p > remainder), p = 1, num_bins)])
        test_diagnosis = .all. (histogram%bin_count() .equalsExpected. block_distribution)
      end associate
    end associate

  end function

end module histogram_test_m
