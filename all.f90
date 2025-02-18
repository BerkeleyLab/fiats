module neural_network_m
  implicit none

  type neural_network_t(k)
    integer, kind :: k = kind(1.)
    real(k), allocatable :: biases_(:)
  end type

  type, extends(neural_network_t) ::  trainable_network_t(m)
    integer, kind :: m = kind(1.)
  end type

  interface neural_network_t

    module function neural_network(biases)
      implicit none
      real biases(:)
      type(neural_network_t) neural_network
    end function

  end interface

  interface trainable_network_t
    module function default_real_network(neural_network) result(trainable_network)
      implicit none
      type(neural_network_t) :: neural_network
      type(trainable_network_t) trainable_network
    end function
  end interface

contains
  module procedure default_real_network
    trainable_network%neural_network_t = neural_network
  end procedure
end module
  use neural_network_m
  implicit none
contains
  function trainable_network()
    type(trainable_network_t) trainable_network
    trainable_network = trainable_network_t(neural_network_t(biases = [0.]))
  end function
end 
