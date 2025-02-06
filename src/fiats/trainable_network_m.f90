module trainable_network_m
  use neural_network_m
  implicit none

  type, extends(neural_network_t) ::  trainable_network_t(m)
    integer, kind :: m = kind(1.)
  end type

contains

  function default_real_network(neural_network) result(trainable_network)
    implicit none
    type(neural_network_t) neural_network
    type(trainable_network_t) trainable_network
    trainable_network%neural_network_t = neural_network
  end function 

  function perturbed_identity_network() result(trainable_network)
    type(trainable_network_t) trainable_network
    integer i
    trainable_network = default_real_network( default_real_construct_from_components ( &
      nodes=[1,3,1], weights=reshape([(0.,i=1,18)],[3,3,2]), biases=reshape([(0.,i=1,18)],[3,2]) &
    ) )
  end function

end module trainable_network_m
