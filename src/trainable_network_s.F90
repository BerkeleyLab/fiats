submodule(trainable_network_m) trainable_network_s
  implicit none 
contains
  module procedure default_real_network
    trainable_network%neural_network_t = neural_network
    trainable_network%workspace_ = workspace_t(neural_network)
  end procedure
end submodule
