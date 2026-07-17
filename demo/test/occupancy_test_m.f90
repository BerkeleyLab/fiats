! Copyright (c) 2022-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

module occupancy_test_m
  !! Unit test for the occupancy subroutine
  use iso_fortran_env, only : int64
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(//) &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.expect.) &
    ,operator(.equalsExpected.) &
    ,passing_test &
    ,string_t &
    ,test_t &
    ,test_result_t &
    ,test_description_t &
    ,test_diagnosis_t
  use fiats_m, only : tensor_t
  use phase_space_bin_m, only : phase_space_bin_t
  use occupancy_m, only : occupancy_t
  implicit none

  private
  public :: occupancy_test_t

  type, extends(test_t) :: occupancy_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The occupancy_t type" 
  end function

  function results() result(test_results)
    type(occupancy_test_t) occupancy_test
    type(test_result_t), allocatable :: test_results(:)

    test_results = occupancy_test%run([ &
       test_description_t("occupying a prescribed set of bins", check_bin_occupation) &
    ])
  end function

  type(test_diagnosis_t) function check_bin_occupation() result(test_diagnosis)
      
    type(tensor_t), allocatable :: tensors(:)
    type(occupancy_t) occupancy
    real, allocatable :: components(:,:)
    integer, parameter :: bins = 3
    integer(int64) i

     ! Define tensors with the depicted tensor counts in a 2D slice of a 9D bin space:
!    |----|----|----|
!    |    |    |  1 |
!    |----|----|----|
!    |    | 3  |    |
!    |----|----|----|
!    | 2  |    |    |
!    |----|----|----|    plus one extra tensor to extablish the ranges ofthe other components
    tensors = [ &
       tensor_t([ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]) & 
      ,tensor_t([ 1.5, 1.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]) & 

      ,tensor_t([ 2.5, 2.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]) & 
      ,tensor_t([ 2.5, 2.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]) & 
      ,tensor_t([ 2.5, 2.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]) & 

      ,tensor_t([ 4.0, 4.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]) & 

      ,tensor_t([ 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0]) & 
    ]

   associate( &
      num_tensors => size(tensors) &
     ,num_components => size(tensors(1)%values()) &
   )
     call_julienne_assert(.all. [( size(tensors(i)%values()), i=2,num_tensors )] .equalsExpected. num_tensors)

     allocate(components(num_tensors, num_components))

     do concurrent(integer :: tensor = 1:num_tensors)
       components(tensor,:) = tensors(tensor)%values()
     end do

     associate(bin => [(phase_space_bin_t(tensors(i), minima = minval(components, dim=1), maxima = maxval(components, dim=1), num_bins = bins), i = 1, num_tensors)])

       call occupancy%vacate(dims = [( bins, i = 1, num_components)])

       populate_bins: &
       do i = 1, size(bin)
         if (occupancy%occupied(bin(i)%loc)) cycle
         call occupancy%occupy(bin(i)%loc)
      end do populate_bins
    end associate

    test_diagnosis = passing_test()
    test_diagnosis = test_diagnosis .also. ((int(occupancy%num_bins()) .equalsExpected. int(bins**num_components)))
    test_diagnosis = test_diagnosis .also. ((.expect. occupancy%occupied([[1,1,1, 1,1,1, 1,1,1]])))
    test_diagnosis = test_diagnosis .also. ((.expect. occupancy%occupied([[2,2,1, 1,1,1, 1,1,1]])))
    test_diagnosis = test_diagnosis .also. ((.expect. occupancy%occupied([[3,3,1, 1,1,1, 1,1,1]])))
    test_diagnosis = test_diagnosis .also. ((.expect. occupancy%occupied([[3,3,3, 3,3,3, 3,3,3]])))

  end associate

  end function

end module occupancy_test_m