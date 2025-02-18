module neural_network_m
  use activation_m, only : activation_t
  use julienne_m, only : string_t
  use metadata_m, only : metadata_t
  implicit none

  type neural_network_t(k)
    integer, kind :: k = kind(1.)
    type(metadata_t), private :: metadata_
    real(k), allocatable, private :: weights_(:,:,:), biases_(:,:)
    integer, allocatable, private :: nodes_(:)
    type(activation_t), private :: activation_
  end type

  type workspace_t(k)
    integer, kind :: k = kind(1.)
    real(k), allocatable, dimension(:,:) :: a
    real(k), allocatable, dimension(:,:,:) :: dcdw, vdw, sdw, vdwc, sdwc
    real(k), allocatable, dimension(:,:) :: z, delta, dcdb, vdb, sdb, vdbc, sdbc
  end type

  interface workspace_t

    pure module function default_real_workspace(neural_network) result(workspace)
      implicit none
      type(neural_network_t), intent(in) :: neural_network
      type(workspace_t) workspace
    end function

  end interface

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
