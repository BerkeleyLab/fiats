module trainable_network_m
  use neural_network_m, only : neural_network_t, workspace_t
  implicit none

  type, extends(neural_network_t) ::  trainable_network_t(m)
    integer, kind :: m = kind(1.) 
    type(workspace_t), private :: workspace_
  end type

  interface trainable_network_t 
    pure module function default_real_network(neural_network) result(trainable_network)
      implicit none
      type(neural_network_t), intent(in) :: neural_network
      type(trainable_network_t) trainable_network
    end function 
  end interface

contains
  module procedure default_real_network
    trainable_network%neural_network_t = neural_network
    trainable_network%workspace_ = workspace_t(neural_network)
  end procedure
end module
