! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

module asymmetric_network_test_m
  !! Define tests in which the desired output depends asymmetrically on the inputs 

  ! External dependencies
  use assert_m
  use julienne_m, only : &
    test_t, test_result_t, vector_test_description_t, test_description_substring, string_t, vector_function_strategy_t

  ! Internal dependencies
  use fiats_m, only : neural_network_t, tensor_t

  implicit none

  private
  public :: asymmetric_network_test_t

  type, extends(test_t) :: asymmetric_network_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  type, extends(vector_function_strategy_t) :: xor_and_2nd_input_t
  contains
    procedure, nopass :: vector_function => xor_and_2nd_input_truth_table 
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "An neural_network_t object encoding an asymmetric XOR-AND-the-2nd-input network"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(xor_and_2nd_input_t) xor_and_2nd_input

    associate( vector_test_description => vector_test_description_t( &
      [ string_t("mapping (true,true) to false"), & 
        string_t("mapping (true,false) to false"), &
        string_t("mapping (false,true) to true"), &
        string_t("mapping (false,false) to false") &
      ], xor_and_2nd_input) &
    )
      associate(substring_in_subject => index(subject(), test_description_substring) /= 0)
        associate(substring_in_description => vector_test_description%contains_text(test_description_substring))
          if (substring_in_subject) then
            test_results = vector_test_description%run()
          else if (any(substring_in_description)) then
            test_results = vector_test_description%run()
            test_results = pack(test_results, test_results%description_contains(string_t(test_description_substring)))
           else
            test_results = [test_result_t::]
          end if
        end associate
      end associate
    end associate
  end function

  function xor_and_2nd_input_network() result(neural_network)
    type(neural_network_t) neural_network
    real, allocatable :: biases(:,:),  weights(:,:,:)
    type(string_t), allocatable :: metadata(:)
    integer, parameter :: n(0:*) = [2,4,4,1]
    integer, parameter :: layers = size(n), n_max = maxval(n)
    integer j, l

    metadata = [string_t("XOR AND 2nd input"),string_t("Damian Rouson"),string_t("2023-02-19"),string_t("step"),string_t("false")]

    allocate(weights(n_max, n_max, layers))
    allocate(biases(n_max, layers))

    l = 1
    call_assert(n(l-1)==2)
      j = 1
      weights(j,1:n(l-1),l) = [1,0]
      j = 2
      weights(j,1:n(l-1),l) = [1,1]
      j = 3
      weights(j,1:n(l-1),l) = [0,1]
      j = 4
      weights(j,1:n(l-1),l) = [0,1]
      biases(1:n(l),l) = [0., -1.99, 0., 0.]
    call_assert(j == n(l))
  
    l = 2
    call_assert(n(l-1)==4)
      j = 1
      weights(j,1:n(l-1),l) = [0,0,0,0]
      j = 2
      weights(j,1:n(l-1),l) = [1,-2,1,0]
      j = 3
      weights(j,1:n(l-1),l) = [0,0,0,0]
      j = 4
      weights(j,1:n(l-1),l) = [0,0,0,1]
      biases(1:n(l),l) = [0,0,0,0]
    call_assert(j == n(l))


    l = 3
    call_assert(n(l-1)==4)
      j = 1
      weights(j,1:n(l-1),l) = [0,1,0,1]
      biases(1:n(l),l) = [-1]
    call_assert(j == n(l))

    neural_network = neural_network_t(metadata, weights, biases, nodes = n)

  end function

  function xor_and_2nd_input_truth_table() result(test_passes)
    logical, allocatable :: test_passes(:)

    type(neural_network_t) asymmetric

    asymmetric = xor_and_2nd_input_network()

    block
      real, parameter :: tolerance = 1.E-08, false = 0., true = 1.
      type(tensor_t) true_true, true_false, false_true, false_false

      true_true = asymmetric%infer(tensor_t([true,true]))
      true_false = asymmetric%infer(tensor_t([true,false]))
      false_true = asymmetric%infer(tensor_t([false,true]))
      false_false = asymmetric%infer(tensor_t([false,false]))

      associate( &
        true_true_outputs => true_true%values(), &
        true_false_outputs => true_false%values(), &
        false_true_outputs => false_true%values(), &
        false_false_outputs => false_false%values() &
      )
        test_passes = [ &
          size(true_true_outputs)==1 .and. abs(true_true_outputs(1) - false) < tolerance, &
          size(true_false_outputs)==1 .and. abs(true_false_outputs(1) - false) < tolerance,  &
          size(false_true_outputs)==1 .and. abs(false_true_outputs(1) - true) < tolerance, &
          size(false_false_outputs)==1 .and. abs(false_false_outputs(1) - false) < tolerance  &
        ]
      end associate
    end block

  end function

end module asymmetric_network_test_m
