module neural_network_m
  implicit none

  type neural_network_t(k)
    integer, kind :: k = kind(1.)
    real(k), allocatable, private :: weights_(:,:,:), biases_(:,:)
  end type

  type, extends(neural_network_t) ::  trainable_network_t(m)
    integer, kind :: m = kind(1.)
  end type

contains

  function default_real_construct_from_components() result(neural_network)
    type(neural_network_t) neural_network
  end function

  function default_real_network(neural_network) result(trainable_network)
    implicit none
    type(neural_network_t) neural_network
    type(trainable_network_t) trainable_network
    trainable_network%neural_network_t = neural_network
  end function 

  function perturbed_identity_network() result(trainable_network)
    type(trainable_network_t) trainable_network
    trainable_network = default_real_network( default_real_construct_from_components ( &
    ) )
  end function

end module
