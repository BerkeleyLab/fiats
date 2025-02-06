module trainable_network_m
  use neural_network_m
  use julienne_m, only : string_t
  use kind_parameters_m, only : default_real
  use training_configuration_m, only : training_configuration_t
  implicit none

  type, extends(neural_network_t) ::  trainable_network_t
  end type

contains

  pure function default_real_network(neural_network) result(trainable_network)
    implicit none
    type(neural_network_t), intent(in) :: neural_network
    type(trainable_network_t) trainable_network
    trainable_network%neural_network_t = neural_network
  end function 

  function perturbed_identity_network(training_configuration, perturbation_magnitude, metadata) &
    result(trainable_network)
    implicit none
    type(training_configuration_t), intent(in) :: training_configuration
    real, intent(in) :: perturbation_magnitude
    type(string_t), intent(in) :: metadata(:)
    type(trainable_network_t) trainable_network

    real, allocatable :: w(:,:,:), b(:,:)

    associate(n=>training_configuration%nodes_per_layer())
      associate(n_max => maxval(n), layers => size(n))
        allocate(w(n_max,n_max,layers-1), source = 0.)
        allocate(b(size(w,1), size(w,3)), source = 0.)
        trainable_network = default_real_network( &
          default_real_construct_from_components(nodes=n, weights=w, biases=b, metadata=metadata) &
        )
      end associate
    end associate

  end function

end module trainable_network_m
