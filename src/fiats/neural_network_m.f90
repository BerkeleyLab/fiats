module neural_network_m
  use activation_m, only : activation_t
  use double_precision_file_m, only : double_precision_file_t
  use kind_parameters_m, only : default_real, double_precision
  use julienne_m, only : file_t, string_t
  use metadata_m, only : metadata_t
  use mini_batch_m, only : mini_batch_t
  use tensor_m, only : tensor_t
  use tensor_map_m, only : tensor_map_t
  implicit none

  type neural_network_t(k)
    integer, kind :: k = default_real 
    type(tensor_map_t(k)), private :: input_map_, output_map_
    type(metadata_t), private :: metadata_
    real(k), allocatable, private :: weights_(:,:,:), biases_(:,:)
    integer, allocatable, private :: nodes_(:)
    type(activation_t), private :: activation_
  end type

  type workspace_t(k)
    integer, kind :: k = default_real
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

    module function default_real_construct_from_components(metadata, weights, biases, nodes, input_map, output_map) &
      result(neural_network)
      implicit none
      type(string_t), intent(in) :: metadata(:)
      real, intent(in) :: weights(:,:,:), biases(:,:)
      integer, intent(in) :: nodes(0:)
      type(tensor_map_t), intent(in), optional :: input_map, output_map
      type(neural_network_t) neural_network
    end function

    module function double_precision_construct_from_components(metadata, weights, biases, nodes, input_map, output_map) &
      result(neural_network)
      implicit none
      type(metadata_t), intent(in) :: metadata
      double precision, intent(in) :: weights(:,:,:), biases(:,:)
      integer, intent(in) :: nodes(0:)
      type(tensor_map_t(double_precision)), intent(in), optional :: input_map, output_map
      type(neural_network_t(double_precision)) neural_network
    end function

  end interface


contains

  module procedure default_real_construct_from_components

    neural_network%metadata_ = metadata_t(metadata(1),metadata(2),metadata(3),metadata(4),metadata(5))
    neural_network%weights_ = weights
    neural_network%biases_ = biases
    neural_network%nodes_ = nodes

    block
      integer i

      if (present(input_map)) then
        neural_network%input_map_ = input_map
      else
        associate(num_inputs => nodes(lbound(nodes,1)))
          associate(default_minima => [(0., i=1,num_inputs)], default_maxima => [(1., i=1,num_inputs)])
            neural_network%input_map_ = tensor_map_t("inputs", default_minima, default_maxima)
          end associate
        end associate
      end if

      if (present(output_map)) then
        neural_network%output_map_ = output_map
      else
        associate(num_outputs => nodes(ubound(nodes,1)))
          associate(default_minima => [(0., i=1,num_outputs)], default_maxima => [(1., i=1,num_outputs)])
            neural_network%output_map_ = tensor_map_t("outputs", default_minima, default_maxima)
          end associate
        end associate
      end if
    end block

    neural_network%activation_ = activation_t(metadata(4)%string())

  end procedure default_real_construct_from_components

  module procedure double_precision_construct_from_components

    neural_network%metadata_ = metadata
    neural_network%weights_ = weights
    neural_network%biases_ = biases
    neural_network%nodes_ = nodes

    block
      integer i

      if (present(input_map)) then
        neural_network%input_map_ = input_map
      else
        associate(num_inputs => nodes(lbound(nodes,1)))
          associate(default_intercept => [(0D0, i=1,num_inputs)], default_slope => [(1D0, i=1,num_inputs)])
            neural_network%input_map_ = tensor_map_t("inputs", default_intercept, default_slope)
          end associate
        end associate
      end if

      if (present(output_map)) then
        neural_network%output_map_ = output_map
      else
        associate(num_outputs => nodes(ubound(nodes,1)))
          associate(default_intercept => [(0D0, i=1,num_outputs)], default_slope => [(1D0, i=1,num_outputs)])
            neural_network%output_map_ = tensor_map_t("outputs", default_intercept, default_slope)
          end associate
        end associate
      end if
    end block

    associate(function_name => metadata%activation_name())
      neural_network%activation_ = activation_t(function_name%string())
    end associate

  end procedure double_precision_construct_from_components

end module neural_network_m
