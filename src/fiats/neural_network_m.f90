module neural_network_m
  implicit none

  type neural_network_t(k)
    integer, kind :: k = kind(1.)
    real(k), allocatable, private :: weights_(:,:,:), biases_(:,:)
    integer, allocatable, private :: nodes_(:)
  end type

contains

  function default_real_construct_from_components(weights, biases, nodes) result(neural_network)
    real, intent(in) :: weights(:,:,:), biases(:,:)
    integer, intent(in) :: nodes(0:)
    type(neural_network_t) neural_network
    neural_network%weights_ = weights
    neural_network%biases_ = biases
    neural_network%nodes_ = nodes
  end function

end module
