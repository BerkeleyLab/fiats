module neural_network_m
  implicit none

  type neural_network_t(k)
    integer, kind :: k = kind(1.)
    real(k), allocatable :: biases
  end type

  type, extends(neural_network_t) ::  trainable_network_t(m)
    integer, kind :: m = kind(1.)
  end type

contains

  function default_real_neural_network(biases)
    real biases
    type(neural_network_t) default_real_neural_network
    default_real_neural_network%biases = biases
  end function

  function default_real_trainable_network(neural_network) result(trainable_network)
    type(neural_network_t) neural_network
    type(trainable_network_t) trainable_network
    trainable_network%neural_network_t = neural_network
  end function

end module

  use neural_network_m
  implicit none
contains
  function trainable_network()
    type(trainable_network_t) trainable_network
    trainable_network = default_real_trainable_network(default_real_neural_network(biases = 0.))
  end function
end 
