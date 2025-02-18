module neural_network_m
  use julienne_string_m
  implicit none

  type metadata_t
    type(string_t) modelName_, modelAuthor_, compilationDate_, activationFunction_, usingSkipConnections_
  end type

  type neural_network_t(k)
    integer, kind :: k = kind(1.)
    type(metadata_t), private :: metadata_
    real(k), allocatable, private :: weights_(:,:,:), biases_(:,:)
    integer, allocatable, private :: nodes_(:)
  end type

  interface neural_network_t

    module function default_real_construct_from_components(metadata, weights, biases, nodes) &
      result(neural_network)
      implicit none
      type(string_t), intent(in) :: metadata(:)
      real, intent(in) :: weights(:,:,:), biases(:,:)
      integer, intent(in) :: nodes(0:)
      type(neural_network_t) neural_network
    end function

  end interface

  type, extends(neural_network_t) ::  trainable_network_t(m)
    integer, kind :: m = kind(1.)
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
  end procedure
end module
