  implicit none

  type neural_network_t(k)
    integer, kind :: k = kind(1.)
    real(k), allocatable :: weights_
  end type

  type, extends(neural_network_t) ::  trainable_network_t(m)
    integer, kind :: m = kind(1.)
  end type

contains

  type(trainable_network_t) function default_real_network()
    default_real_network%neural_network_t = neural_network_t(weights_=0.)
  end function 

  type(trainable_network_t) function perturbed_identity_network()
    perturbed_identity_network = default_real_network()
  end function

end
