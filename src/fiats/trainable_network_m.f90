! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module trainable_network_m
  use neural_network_m, only : neural_network_t
  use input_output_pair_m, only : input_output_pair_t
  use julienne_m, only : string_t
  use kind_parameters_m, only : default_real
  use mini_batch_m, only : mini_batch_t
  use training_configuration_m, only : training_configuration_t
  use tensor_map_m, only : tensor_map_t
  implicit none

  private
  public :: trainable_network_t 

  type, extends(neural_network_t) ::  trainable_network_t(m)
    integer, kind :: m = default_real
    private
  end type

  interface trainable_network_t 

    module function perturbed_identity_network(training_configuration, perturbation_magnitude, metadata, input_map, output_map) &
      result(trainable_network)
      implicit none
      type(training_configuration_t), intent(in) :: training_configuration
      real, intent(in) :: perturbation_magnitude
      type(string_t), intent(in) :: metadata(:)
      type(tensor_map_t) input_map, output_map
      type(trainable_network_t) trainable_network
    end function

  end interface

contains

    pure function default_real_network(neural_network) result(trainable_network)
      implicit none
      type(neural_network_t), intent(in) :: neural_network
      type(trainable_network_t) trainable_network
      trainable_network%neural_network_t = neural_network
    end function 

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
          trainable_network = default_real_network( &
            neural_network_t(nodes=n, weights=w, biases=b, metadata=metadata, input_map=input_map, output_map=output_map) &
          )
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

end module trainable_network_m
