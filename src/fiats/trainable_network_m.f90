! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module trainable_network_m
  use neural_network_m, only : neural_network_t
  use julienne_m, only : string_t
  use kind_parameters_m, only : default_real
  use training_configuration_m, only : training_configuration_t
  use tensor_map_m, only : tensor_map_t
  implicit none

  type, extends(neural_network_t) ::  trainable_network_t(m)
    integer, kind :: m = default_real
    private
  end type

contains

  pure function default_real_network(neural_network) result(trainable_network)
    implicit none
    type(neural_network_t), intent(in) :: neural_network
    type(trainable_network_t) trainable_network
    trainable_network%neural_network_t = neural_network
  end function 

  function perturbed_identity_network(training_configuration, perturbation_magnitude, metadata, input_map, output_map) &
      result(trainable_network)
      implicit none
      type(training_configuration_t), intent(in) :: training_configuration
      real, intent(in) :: perturbation_magnitude
      type(string_t), intent(in) :: metadata(:)
      type(tensor_map_t) input_map, output_map
      type(trainable_network_t) trainable_network

    real, allocatable :: w(:,:,:), b(:,:)

    associate(n=>training_configuration%nodes_per_layer())
      associate(n_max => maxval(n), layers => size(n))
        allocate(w(n_max,n_max,layers-1), source = 0.)
        allocate(b(size(w,1), size(w,3)), source = 0.)
        trainable_network = default_real_network( &
          neural_network_t(nodes=n, weights=w, biases=b, metadata=metadata, input_map=input_map, output_map=output_map) &
        )
      end associate
    end associate

  end function

end module trainable_network_m
