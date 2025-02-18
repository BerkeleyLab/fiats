submodule(neural_network_m) neural_network_s
  use double_precision_string_m, only : double_precision_string_t
  use kind_parameters_m, only : double_precision
  implicit none

  integer, parameter :: input_layer = 0 

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

end submodule neural_network_s
