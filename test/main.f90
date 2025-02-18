  use julienne_m, only : string_t
  use fiats_m, only : trainable_network_t, neural_network_t
  implicit none
contains
  function perturbed_identity_network(perturbation_magnitude) result(trainable_network)
    type(trainable_network_t) trainable_network
    real, intent(in) :: perturbation_magnitude
    integer, parameter :: nodes_per_layer(*) = [2, 2, 2, 2]
    integer, parameter :: max_n = maxval(nodes_per_layer), layers = size(nodes_per_layer)
    real, parameter :: identity(*,*,*) = &
      reshape([real:: [1,0], [0,1] ,[1,0], [0,1], [1,0], [0,1]], [max_n, max_n, layers-1])
    real harvest(size(identity,1), size(identity,2), size(identity,3))

    call random_number(harvest)
    harvest = perturbation_magnitude*harvest

    trainable_network = trainable_network_t( neural_network_t( &
      nodes = nodes_per_layer, &
      weights = identity + harvest , & 
      biases = reshape([real:: [0,0], [0,0], [0,0]], [max_n, layers-1]), &
      metadata = [string_t("Identity"), string_t("Damian Rouson"), string_t("2023-09-18"), string_t("relu"), string_t("false")] &
    ))
  end function
end 
