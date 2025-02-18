module neural_network_m
  use julienne_m, only : string_t
  use metadata_m, only : metadata_t
  implicit none

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

end module neural_network_m
