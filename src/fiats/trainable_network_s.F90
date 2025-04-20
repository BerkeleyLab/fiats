! Copyright (c) 2023-2025, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

submodule(trainable_network_m) trainable_network_s
  implicit none 

contains

  module procedure default_real_construct_from_components
    trainable_network%neural_network_t = neural_network_t(metadata, weights, biases, nodes, input_map, output_map)
    trainable_network%workspace_ = workspace_t(trainable_network%neural_network_t)
  end procedure

  module procedure default_real_network
    trainable_network%neural_network_t = neural_network
    trainable_network%workspace_ = workspace_t(neural_network)
  end procedure

  module procedure default_real_from_json
    associate(neural_network => neural_network_t(file))
      trainable_network%neural_network_t = neural_network
      trainable_network%workspace_ = workspace_t(neural_network)
    end associate
  end procedure

  module procedure default_real_train
    call self%learn(mini_batches_arr, cost, adam, learning_rate, self%workspace_)
  end procedure

  module procedure default_real_map_to_training_ranges
    associate(inputs => input_output_pair%inputs(), expected_outputs => input_output_pair%expected_outputs())
      associate( &
         normalized_inputs  => self%map_to_input_range(inputs), &
         normalized_outputs => self%map_from_output_range(expected_outputs) &

      )
        normalized_input_output_pair = input_output_pair_t(normalized_inputs, normalized_outputs)
      end associate
    end associate
  end procedure

  module procedure perturbed_identity_network

    integer k, l
    real, allocatable :: identity(:,:,:), w_harvest(:,:,:), b_harvest(:,:)

    associate(n=>training_configuration%nodes_per_layer())
      associate(n_max => maxval(n), layers => size(n))

        identity = reshape( [( [(e(k,n_max), k=1,n_max)], l = 1, layers-1 )], [n_max, n_max, layers-1])
        allocate(w_harvest, mold = identity)
        allocate(b_harvest(size(identity,1), size(identity,3)))
        call random_number(w_harvest)
        call random_number(b_harvest)

        associate( &
          w => identity + perturbation_magnitude*(w_harvest-0.5)/0.5, &
          b => perturbation_magnitude*(b_harvest-0.5)/0.5 &
        )
#ifndef NAGFOR
          trainable_network = trainable_network_t( &
            neural_network_t(nodes=n, weights=w, biases=b, metadata=metadata, input_map=input_map, output_map=output_map) &
          )
#else
          ! manually inline the default_real_network() function invoked by the above user-defined trainable_network_t constructor
          associate(  &
           neural_network =>  &
           neural_network_t(nodes=n, weights=w, biases=b, metadata=metadata, input_map=input_map, output_map=output_map) &
          )
            trainable_network%neural_network_t = neural_network
            trainable_network%workspace_ = workspace_t(neural_network)
          end associate
#endif
        end associate
      end associate
    end associate

  contains

    pure function e(j,n) result(unit_vector)
      integer, intent(in) :: j, n
      integer k
      real, allocatable :: unit_vector(:)
      unit_vector = real([(merge(1,0,j==k),k=1,n)])
    end function

  end procedure
end submodule trainable_network_s
