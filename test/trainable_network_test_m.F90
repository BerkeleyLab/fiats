! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

module trainable_network_test_m
  !! Define inference tests and procedures required for reporting results

  ! External dependencies
  use assert_m
  use julienne_m, only : &
     bin_t &
    ,call_julienne_assert_ &
    ,operator(.all.) &
    ,operator(.approximates.) &
    ,operator(.within.) &
    ,string_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t

  ! Internal dependencies
  use fiats_m, only : trainable_network_t, neural_network_t, tensor_t, input_output_pair_t, mini_batch_t, shuffle
  implicit none

  private
  public :: trainable_network_test_t

  type, extends(test_t) :: trainable_network_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  real, parameter :: false = 0., true = 1.

  abstract interface

    function map_i(inputs) result(expected_outputs)
      import tensor_t
      type(tensor_t), intent(in) :: inputs
      type(tensor_t) expected_outputs
    end function

  end interface

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A trainable_network_t object"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(trainable_network_test_t) trainable_network_test

    test_results = trainable_network_test%run([ &
       test_description_t("preserving an identity map with 2 hidden layers", preserve_identity) &
      ,test_description_t("learning identity with Adam from perturbed-identity initial condition", learn_identity) &
      ,test_description_t("learning a 2-hidden-layer AND gate from a skewed AND training data", learn_and_from_skewed_data) &
      ,test_description_t("learning NOT-AND from skewed NOT-AND data", learn_not_and_from_skewed_data) &
      ,test_description_t("learning OR from symmetric OR-gate data and random initial weights", learn_or_from_random_weights) &
      ,test_description_t("learning XOR from symmetric XOR-gate data and random initial weights", learn_xor_from_random_weights) &
    ])
  end function

  subroutine print_truth_table(gate_name, gate_function_ptr, test_inputs, actual_outputs)
    !! Usage: 
    !!   procedure(map_i), pointer :: xor_ptr
    !!   xor_ptr => xor
    !!   call print_truth_table("XOR", xor_ptr, test_inputs, actual_outputs)
    character(len=*), intent(in) :: gate_name
    procedure(map_i), intent(in), pointer :: gate_function_ptr
    type(tensor_t), intent(in), dimension(:) :: test_inputs, actual_outputs
    type(tensor_t) expected_outputs
    integer i

    call_julienne_assert(size(test_inputs) .equalsExpected. size(actual_outputs))

    print *,"_______" // gate_name // "_______"

    do i = 1, size(test_inputs)
      expected_outputs = gate_function_ptr(test_inputs(i))
      print *,test_inputs(i)%values(), "-->", expected_outputs%values(), ":", actual_outputs(i)%values()
    end do
  end subroutine

  function two_zeroed_hidden_layers() result(trainable_network)
    type(trainable_network_t) trainable_network
    integer, parameter :: inputs = 2, outputs = 1, hidden = 3 ! number of neurons in input, output, and hidden layers
    integer, parameter :: neurons(*) = [inputs, hidden, hidden, outputs] ! neurons per layer
    integer, parameter :: max_neurons = maxval(neurons), layers=size(neurons) ! max layer width, number of layers
    real w(max_neurons, max_neurons, layers-1), b(max_neurons, max_neurons)

    w = 0.
    b = 0.

    trainable_network = trainable_network_t( neural_network_t( &
      nodes = neurons, weights = w, biases = b &
     ,metadata = [string_t("2-hide|3-wide"), string_t("Rouson"), string_t("2023-06-30"), string_t("sigmoid"), string_t("false")] &
    ))
  end function

  function two_random_hidden_layers() result(trainable_network)
    type(trainable_network_t) trainable_network
    integer, parameter :: inputs = 2, outputs = 1, hidden = 3 ! number of neurons in input, output, and hidden layers
    integer, parameter :: neurons(*) = [inputs, hidden, hidden, outputs] ! neurons per layer
    integer, parameter :: max_neurons = maxval(neurons), layers=size(neurons) ! max layer width, number of layers
    real w(max_neurons, max_neurons, layers-1), b(max_neurons, max_neurons)

    call random_number(b)
    call random_number(w)
    b = 2*b
    w = 2*w

    trainable_network = trainable_network_t( neural_network_t( &
      nodes = neurons, weights = w, biases = b &
      ,metadata = [string_t("2-hide|3-wide"), string_t("Rouson"), string_t("2023-06-30"), string_t("sigmoid"), string_t("false")] &
    ))
  end function

  function learn_and_from_skewed_data() result(test_diagnosis)
    !! AND truth table: [(true,true), (false,true), (true,false), (false,false)] -> [ true, false, false, false]
    type(test_diagnosis_t) test_diagnosis
    type(mini_batch_t), allocatable :: mini_batches(:)
    type(tensor_t), allocatable, dimension(:,:) :: training_inputs, training_outputs
    type(tensor_t), allocatable, dimension(:) :: tmp, tmp2, test_inputs, expected_outputs, actual_outputs
    type(trainable_network_t) trainable_network
    real, parameter :: tolerance = 1.E-02
    real, allocatable :: harvest(:,:,:)
    integer, parameter :: num_inputs=2, mini_batch_size = 1, num_iterations=20000
    integer batch, iter, i

    allocate(harvest(num_inputs, mini_batch_size, num_iterations))
    call random_number(harvest)
    harvest = 2.*(harvest - 0.5) ! skew toward more input values being true


    ! The following temporary copies are required by gfortran bug 100650 and possibly 49324
    ! See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100650 and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=49324
    tmp = [([(tensor_t(merge(true, false, harvest(:,batch,iter) < 0.5E0)), batch=1, mini_batch_size)], iter=1, num_iterations)]
    training_inputs = reshape(tmp, [mini_batch_size, num_iterations])

    tmp2 = [([(and(training_inputs(batch, iter)), batch = 1, mini_batch_size)], iter = 1, num_iterations )]
    training_outputs = reshape(tmp2, [mini_batch_size, num_iterations])

    mini_batches = [(mini_batch_t(input_output_pair_t(training_inputs(:,iter), training_outputs(:,iter))), iter=1, num_iterations)]        
    trainable_network = two_zeroed_hidden_layers()

    call trainable_network%train(mini_batches, adam=.false., learning_rate=1.5)

    test_inputs = [tensor_t([true,true]), tensor_t([false,true]), tensor_t([true,false]), tensor_t([false,false])]
    expected_outputs = [(and(test_inputs(i)), i=1, size(test_inputs))]
    actual_outputs = trainable_network%infer(test_inputs)
    test_diagnosis = .all. [(actual_outputs(i)%values() .approximates. expected_outputs(i)%values() .within. tolerance, i=1,size(actual_outputs))]

  contains

    elemental function and(inputs_object) result(expected_outputs_object)
      type(tensor_t), intent(in) :: inputs_object 
      type(tensor_t) expected_outputs_object 
      expected_outputs_object = tensor_t([merge(true, false, sum(inputs_object%values()) > 1.99)])
    end function

  end function

  function learn_not_and_from_skewed_data() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(mini_batch_t), allocatable :: mini_batches(:)
    type(tensor_t), allocatable :: training_inputs(:,:), tmp(:), test_inputs(:)
    type(tensor_t), allocatable :: training_outputs(:,:), expected_outputs(:), tmp2(:)
    type(trainable_network_t) trainable_network
    type(tensor_t), allocatable :: actual_outputs(:)
    real, parameter :: tolerance = 1.E-02
    real, allocatable :: harvest(:,:,:)
    integer, parameter :: num_inputs=2, mini_batch_size = 1, num_iterations=30000
    integer batch, iter, i

    allocate(harvest(num_inputs, mini_batch_size, num_iterations))
    call random_number(harvest)
    harvest = 2.*(harvest - 0.5) ! skew toward more input values being true

    ! The following temporary copies are required by gfortran bug 100650 and possibly 49324
    ! See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100650 and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=49324
    tmp = [([(tensor_t(merge(true, false, harvest(:,batch,iter) < 0.5E0)), batch=1, mini_batch_size)], iter=1, num_iterations)]
    training_inputs = reshape(tmp, [mini_batch_size, num_iterations])

    tmp2 = [([(not_and(training_inputs(batch, iter)), batch = 1, mini_batch_size)], iter = 1, num_iterations )]
    training_outputs = reshape(tmp2, [mini_batch_size, num_iterations])

    mini_batches = [(mini_batch_t(input_output_pair_t(training_inputs(:,iter), training_outputs(:,iter))), iter=1, num_iterations)]        
    trainable_network = two_zeroed_hidden_layers()

    call trainable_network%train(mini_batches, adam=.false., learning_rate=1.5)

    test_inputs = [tensor_t([true,true]), tensor_t([false,true]), tensor_t([true,false]), tensor_t([false,false])]
    expected_outputs = [(not_and(test_inputs(i)), i=1, size(test_inputs))]
    actual_outputs = trainable_network%infer(test_inputs)
    test_diagnosis = .all. [(actual_outputs(i)%values() .approximates. expected_outputs(i)%values() .within. tolerance, i=1,size(actual_outputs))]

  contains
    
    function not_and(inputs) result(expected_outputs)
       type(tensor_t), intent(in) :: inputs
       type(tensor_t) expected_outputs
       expected_outputs = tensor_t([merge(true, false, sum(inputs%values()) < 2.)])
    end function

  end function

  function learn_or_from_random_weights() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(mini_batch_t), allocatable :: mini_batches(:)
    type(tensor_t), allocatable :: training_inputs(:,:), test_inputs(:), actual_outputs(:)
    type(tensor_t), allocatable :: training_outputs(:,:), expected_outputs(:)
    type(trainable_network_t) trainable_network
    real, parameter :: tolerance = 1.E-02
    real, allocatable :: harvest(:,:,:)
    integer, parameter :: num_inputs=2, mini_batch_size = 1, num_iterations=50000
    integer batch, iter, i

    test_diagnosis = test_diagnosis_t(.false.,"")

    allocate(harvest(num_inputs, mini_batch_size, num_iterations))
    call random_number(harvest)

    allocate(training_inputs(mini_batch_size, num_iterations))
    allocate(training_outputs(mini_batch_size, num_iterations))
    do concurrent(batch=1:mini_batch_size, iter=1:num_iterations)
      training_inputs(batch, iter) = tensor_t(merge(true, false, harvest(:,batch,iter) < 0.5E0))
      training_outputs(batch, iter) = or(training_inputs(batch, iter))
    end do

    allocate(mini_batches(size(training_inputs,1)*num_iterations))
    do concurrent(iter=1:num_iterations)
      mini_batches(iter) = mini_batch_t(input_output_pair_t(training_inputs(:,iter), training_outputs(:,iter)))
    end do

    trainable_network = two_random_hidden_layers()

    call trainable_network%train(mini_batches, adam=.false., learning_rate=1.5)

    test_inputs = [tensor_t([true,true]), tensor_t([false,true]), tensor_t([true,false]), tensor_t([false,false])]
    expected_outputs = [(or(test_inputs(i)), i=1, size(test_inputs))]
    actual_outputs = trainable_network%infer(test_inputs)
    test_diagnosis = .all. [(actual_outputs(i)%values() .approximates. expected_outputs(i)%values() .within. tolerance, i=1,size(actual_outputs))]

  contains
    
    pure function or(inputs) result(expected_outputs)
       type(tensor_t), intent(in) :: inputs
       type(tensor_t) expected_outputs
       expected_outputs = tensor_t([merge(true, false, sum(inputs%values()) > 0.99)])
    end function

  end function

  function learn_xor_from_random_weights() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(mini_batch_t), allocatable :: mini_batches(:)
    type(tensor_t), allocatable, dimension(:,:) :: training_inputs, training_outputs 
    type(tensor_t), allocatable, dimension(:) :: actual_outputs, test_inputs, expected_outputs
    type(trainable_network_t) trainable_network
    real, parameter :: tolerance = 1.E-02
    real, allocatable :: harvest(:,:,:)

#ifdef __flang__
      !! Reducing num_iterations yields a less robust test, but moving away from local minima by
      !! increasing num_iterations causes this test to crash when compiled with the flang or ifx compilers.
    integer, parameter :: num_inputs=2, mini_batch_size = 1, num_iterations=50000
#elif defined __INTEL_COMPILER
    integer, parameter :: num_inputs=2, mini_batch_size = 1, num_iterations=49000
#else
    integer, parameter :: num_inputs=2, mini_batch_size = 1, num_iterations=500000
      !! Depending on where in the random-number sequence the weights start, this test can pass for lower
      !! numbers of iterations, e.g., 400000. Using more iterations gives more robust convergence.
#endif
    integer batch, iter, i

    allocate(harvest(num_inputs, mini_batch_size, num_iterations))
    call random_number(harvest)

    allocate(training_inputs(mini_batch_size, num_iterations))
    allocate(training_outputs(mini_batch_size, num_iterations))
    do concurrent(batch=1:mini_batch_size, iter=1:num_iterations)
      training_inputs(batch, iter) = tensor_t(merge(true, false, harvest(:,batch,iter) < 0.5E0))
      training_outputs(batch, iter) = local_xor(training_inputs(batch, iter))
    end do

    test_diagnosis = test_diagnosis_t(.false., "")

    allocate(mini_batches(size(training_inputs,1)*num_iterations))
    do concurrent(iter=1:num_iterations)
      mini_batches(iter) = mini_batch_t(input_output_pair_t(training_inputs(:,iter), training_outputs(:,iter)))
    end do

    trainable_network = two_random_hidden_layers()

    call trainable_network%train(mini_batches, adam=.true., learning_rate=1.5)

    test_inputs = [tensor_t([true,true]), tensor_t([false,true]), tensor_t([true,false]), tensor_t([false,false])]
    expected_outputs = [(local_xor(test_inputs(i)), i=1, size(test_inputs))]
    actual_outputs = trainable_network%infer(test_inputs)
    test_diagnosis = .all. [(actual_outputs(i)%values() .approximates. expected_outputs(i)%values() .within. tolerance, i=1,size(actual_outputs))]

  contains
    
    pure function local_xor(inputs) result(expected_outputs)
      type(tensor_t), intent(in) :: inputs
      type(tensor_t) expected_outputs
      associate(sum_inputs => sum(inputs%values()))
       expected_outputs = tensor_t([merge(true, false, sum_inputs > 0.99 .and. sum_inputs < 1.01)])
      end associate
    end function

  end function

  function perturbed_identity_network(perturbation_magnitude) result(trainable_network)
    type(trainable_network_t) trainable_network
    real, intent(in) :: perturbation_magnitude
    integer, parameter :: nodes_per_layer(*) = [2, 2, 2, 2]
    integer, parameter :: max_n = maxval(nodes_per_layer), layers = size(nodes_per_layer)
    real, parameter :: identity(*,*,*) = &
      reshape([real:: [1,0], [0,1] ,[1,0], [0,1], [1,0], [0,1]], [max_n, max_n, layers-1])
    real harvest(size(identity,1), size(identity,2), size(identity,3))

    call random_number(harvest)
    harvest = perturbation_magnitude*harvest

    trainable_network = trainable_network_t( neural_network_t( &
      nodes = nodes_per_layer, &
      weights = identity + harvest , & 
      biases = reshape([real:: [0,0], [0,0], [0,0]], [max_n, layers-1]), &
      metadata = [string_t("Identity"), string_t("Damian Rouson"), string_t("2023-09-18"), string_t("relu"), string_t("false")] &
    ))
  end function

  function preserve_identity() result(test_diagnosis)
    type(test_diagnosis_t)test_diagnosis
    type(mini_batch_t), allocatable :: mini_batches(:)
    type(input_output_pair_t), allocatable :: input_output_pairs(:)
    type(tensor_t), allocatable :: inputs(:)
    type(trainable_network_t)  trainable_network
    type(bin_t), allocatable :: bins(:)
    real, allocatable :: cost(:)
    integer, parameter :: num_pairs = 100, num_epochs = 100, n_bins = 3
    integer i, bin, epoch

    trainable_network = perturbed_identity_network(perturbation_magnitude=0.)

    associate(num_inputs => trainable_network%num_inputs(), num_outputs => trainable_network%num_outputs())

      call_julienne_assert(num_inputs .equalsExpected. num_outputs)
      inputs = [(tensor_t(real([i,2*i])/num_pairs), i = 1, num_pairs)]
      associate(outputs => inputs)
        input_output_pairs = input_output_pair_t(inputs, outputs)
      end associate
      bins = [(bin_t(num_items=num_pairs, num_bins=n_bins, bin_number=bin), bin = 1, n_bins)]

      do epoch = 1,num_epochs
        mini_batches = [(mini_batch_t(input_output_pairs(bins(bin)%first():bins(bin)%last())), bin = 1, size(bins))]
        call trainable_network%train(mini_batches, cost, adam=.false., learning_rate=1.5)
      end do

      block
        real, parameter :: tolerance = 1.E-06
#if defined _CRAYFTN || __GFORTRAN__
        type(tensor_t), allocatable :: network_outputs(:)
        network_outputs = trainable_network%infer(inputs)
#else
        associate(network_outputs => trainable_network%infer(inputs))
#endif
          test_diagnosis = .all. [(network_outputs(i)%values() .approximates. inputs(i)%values() .within. tolerance, i=1,size(inputs))]
#if defined _CRAYFTN || __GFORTRAN__
#else
        end associate
#endif
      end block

   end associate

  end function

  function learn_identity() result(test_diagnosis)
    ! test that a network that represents a randomly perturbed identity mapping converges to an identity,
    ! (i.e., mapping inputs to outputs identically). This test operates at the edge of a radius of
    ! non-convergence, i.e., for the given size training data set, decrementing num_epochs or num_bins
    ! or negating adam or not shuffling doesn't converge within the specified output-value tolerance.
    type(test_diagnosis_t) test_diagnosis
    type(mini_batch_t), allocatable :: mini_batches(:)
    type(input_output_pair_t), allocatable :: input_output_pairs(:)
    type(tensor_t), allocatable :: inputs(:)
    type(trainable_network_t)  trainable_network
    type(bin_t), allocatable :: bins(:)
    real, allocatable :: cost(:)
    integer, parameter :: num_pairs = 6
    integer, parameter :: num_epochs = 180
    integer, parameter :: num_bins = 5 
    integer i, bin, epoch

    trainable_network = perturbed_identity_network(perturbation_magnitude=0.1)

    associate(num_inputs => trainable_network%num_inputs(), num_outputs => trainable_network%num_outputs())

      call_julienne_assert(num_inputs .equalsExpected. num_outputs)
      inputs = [(tensor_t(real([i,2*i])/(2*num_pairs)), i = 1, num_pairs)]
      associate(outputs => inputs)
        input_output_pairs = input_output_pair_t(inputs, outputs)
      end associate
      bins = [(bin_t(num_items=num_pairs, num_bins=num_bins, bin_number=bin), bin = 1, num_bins)]

      do epoch = 1,num_epochs
        call shuffle(input_output_pairs)
        mini_batches = [(mini_batch_t(input_output_pairs(bins(bin)%first():bins(bin)%last())), bin = 1, size(bins))]
        call trainable_network%train(mini_batches, cost, adam=.true., learning_rate=1.5)
      end do

      block
        real, parameter :: tolerance = 1.E-06
#if defined _CRAYFTN || __GFORTRAN__
        type(tensor_t), allocatable :: network_outputs(:)
        network_outputs = trainable_network%infer(inputs)
#else
        associate(network_outputs => trainable_network%infer(inputs))
#endif
          test_diagnosis = .all. [(network_outputs(i)%values() .approximates. inputs(i)%values() .within. tolerance, i=1,size(inputs))]
#if defined _CRAYFTN || __GFORTRAN__
#else
        end associate
#endif
      end block

   end associate

  end function

end module trainable_network_test_m
