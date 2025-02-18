module neural_network_m
  implicit none

  type string_t
    character(len=:), allocatable :: string_
  end type

  type metadata_t
    type(string_t) modelName_, modelAuthor_, compilationDate_, activationFunction_, usingSkipConnections_
  end type

  type neural_network_t(k)
    integer, kind :: k = kind(1.)
    type(metadata_t), private :: metadata_
    real(k), allocatable, private :: biases_(:,:)
    integer, allocatable, private :: nodes_(:)
  end type

  interface neural_network_t

    module function neural_network(metadata, biases, nodes)
      implicit none
      type(string_t), intent(in) :: metadata(:)
      real, intent(in) :: biases(:,:)
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
  use neural_network_m
  implicit none
contains
  function trainable_network()
    type(trainable_network_t) trainable_network
    integer, parameter :: nodes_per_layer(*) = [2, 2, 2, 2]
    integer, parameter :: max_n = maxval(nodes_per_layer), layers = size(nodes_per_layer)
    real, parameter :: identity(*,*,*) = &
      reshape([real:: [1,0], [0,1] ,[1,0], [0,1], [1,0], [0,1]], [max_n, max_n, layers-1])

    trainable_network = trainable_network_t( neural_network_t( &
      nodes = nodes_per_layer, &
      biases = reshape([real:: [0,0], [0,0], [0,0]], [max_n, layers-1]), &
      metadata = [string_t("Identity"), string_t("Damian Rouson"), string_t("2023-09-18"), string_t("relu"), string_t("false")] &
    ))
  end function
end 
