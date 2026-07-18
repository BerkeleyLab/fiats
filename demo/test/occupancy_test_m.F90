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
    ,test_t &
    ,test_result_t &
    ,test_description_t &
    ,test_diagnosis_t
  use fiats_m, only : tensor_t
  use occupancy_m, only : occupancy_t, phase_space_bin_t
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

  function check_bin_occupation() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
      
    type(tensor_t), allocatable :: tensors(:)
    real, allocatable :: components(:,:)
    integer, parameter :: bins_per_dimension = 3
    integer(int64) t

    ! Define tensors with the tensor counts depicted below in a 2D slice of a 9D bin space.
    ! Add one extra tensor to extablish the ranges of the other components

    tensors = [ &
       tensor_t([ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]) & !    |----|----|----|
      ,tensor_t([ 1.5, 1.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]) & !    |    |    |  1 |
      ,tensor_t([ 2.5, 2.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]) & !    |----|----|----|
      ,tensor_t([ 2.5, 2.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]) & !    |    | 3  |    |
      ,tensor_t([ 2.5, 2.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]) & !    |----|----|----|
      ,tensor_t([ 4.0, 4.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]) & !    | 2  |    |    |
      ,tensor_t([ 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0]) & !    |----|----|----|
    ]

    associate( &
       num_tensors => size(tensors) &
      ,num_components => size(tensors(1)%values()) &
    )
      call_julienne_assert(.all. [( size(tensors(t)%values()), i=2,num_tensors )] .equalsExpected. num_tensors)

      allocate(components(num_tensors, num_components))

      do concurrent(integer :: t = 1:num_tensors)
        components(t,:) = tensors(t)%values()
      end do

      block 
        type(occupancy_t) occupancy
        integer c

        associate( &
           minima => [(minval(components(:,c)), c = 1, num_components)] &
          ,maxima => [(maxval(components(:,c)), c = 1, num_components)] &
        )
          associate(bin => [(phase_space_bin_t(tensors(t), minima = minima, maxima = maxima, bins_per_dimension = bins_per_dimension), t = 1, num_tensors)])
            
            occupancy = occupancy_t(minima = minima, maxima = maxima, bins_per_dimension = bins_per_dimension)
        
            do t = 1, num_tensors
             if (.not. occupancy%occupied(bin(t))) call occupancy%add(bin(t))
            end do

            test_diagnosis = passing_test()
            test_diagnosis = test_diagnosis .also. (.expect. occupancy%occupied(phase_space_bin_t([1,1,1, 1,1,1, 1,1,1])))
            test_diagnosis = test_diagnosis .also. (.expect. occupancy%occupied(phase_space_bin_t([2,2,1, 1,1,1, 1,1,1])))
            test_diagnosis = test_diagnosis .also. (.expect. occupancy%occupied(phase_space_bin_t([3,3,1, 1,1,1, 1,1,1])))
            test_diagnosis = test_diagnosis .also. (.expect. occupancy%occupied(phase_space_bin_t([3,3,3, 3,3,3, 3,3,3])))
            test_diagnosis = test_diagnosis .also. (occupancy%num_occupied() .equalsExpected. 4) // "number of occupied bins"
          end associate
        end associate
      end block
    end associate
  end function

end module occupancy_test_m