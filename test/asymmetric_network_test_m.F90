! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

module asymmetric_network_test_m
  !! Define tests in which the desired output depends asymmetrically on the inputs 

  ! External dependencies
  use assert_m
  use julienne_m, only : &
     operator(.approximates.) &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.equalsExpected.) &
    ,operator(.within.) &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_t &
    ,test_result_t &
    ,test_description_substring &
    ,string_t

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

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "An neural_network_t object encoding an asymmetric XOR-AND-the-2nd-input network"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

    test_descriptions = [ &
      test_description_t("matching the truth table for XOR-and-2nd-input gate", check_xor_and_2nd_input_truth_table) &
    ]
    associate(substring_in_subject => index(subject(), test_description_substring) /= 0)
      associate(substring_in_test_diagnosis => test_descriptions%contains_text(test_description_substring))
        associate(matching_descriptions => pack(test_descriptions, substring_in_subject .or. substring_in_test_diagnosis))
          test_results = matching_descriptions%run()
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

  function check_xor_and_2nd_input_truth_table() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(neural_network_t) asymmetric
    real, parameter :: tolerance = 1.E-08, false = 0., true = 1.
    type(tensor_t) true_true, true_false, false_true, false_false

    asymmetric = xor_and_2nd_input_network()

    true_true = asymmetric%infer(tensor_t([true,true]))
    true_false = asymmetric%infer(tensor_t([true,false]))
    false_true = asymmetric%infer(tensor_t([false,true]))
    false_false = asymmetric%infer(tensor_t([false,false]))

    associate( &
         true_true_output =>   true_true%values() &
      , true_false_output =>  true_false%values() &
      , false_true_output =>  false_true%values() &
      ,false_false_output => false_false%values() &
    )
      test_diagnosis = &
        (.all. ([size(true_true_output),size(true_false_output),size(false_true_output),size(false_false_output)]  .equalsExpected. 1)) &
        .also. (  true_true_output(1) .approximates. (false) .within. tolerance) &
        .also. ( true_false_output(1) .approximates. (false) .within. tolerance) &
        .also. ( false_true_output(1) .approximates. (true)  .within. tolerance) &
        .also. (false_false_output(1) .approximates. (false) .within. tolerance)
    end associate

  end function

end module asymmetric_network_test_m
