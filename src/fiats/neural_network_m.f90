module neural_network_m
  use activation_m, only : activation_t
  use kind_parameters_m, only : default_real, double_precision
  use julienne_m, only : string_t
  use metadata_m, only : metadata_t
  use tensor_m, only : tensor_t
  use tensor_map_m, only : tensor_map_t
  implicit none

  type neural_network_t(k)
    integer, kind :: k = default_real 
    type(metadata_t), private :: metadata_
    real(k), allocatable, private :: weights_(:,:,:), biases_(:,:)
    integer, allocatable, private :: nodes_(:)
    type(activation_t), private :: activation_
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

contains

  module procedure default_real_construct_from_components
    neural_network%metadata_ = metadata_t(metadata(1),metadata(2),metadata(3),metadata(4),metadata(5))
    neural_network%weights_ = weights
    neural_network%biases_ = biases
    neural_network%nodes_ = nodes
    neural_network%activation_ = activation_t(metadata(4)%string())
  end procedure default_real_construct_from_components

end module neural_network_m
