module trainable_network_test_m
  use julienne_m, only : string_t, bin_t
  use fiats_m, only : trainable_network_t, neural_network_t, tensor_t, input_output_pair_t, mini_batch_t
  implicit none

contains

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

  function preserves_identity_mapping() result(test_passes)
    logical test_passes
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
        associate(network_outputs => trainable_network%infer(inputs))
          test_passes = maxval(abs([(network_outputs(i)%values() - inputs(i)%values(), i=1,num_pairs)])) < tolerance
        end associate
      end block

   end associate

  end function

end module trainable_network_test_m

end 
