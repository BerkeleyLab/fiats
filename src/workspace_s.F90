submodule(neural_network_m) workspace_s
  implicit none

  integer, parameter :: input_layer = 0

contains

  module procedure default_real_workspace

    allocate(workspace%dcdw, mold=neural_network%weights_) ! Gradient of cost function with respect to weights
    allocate(workspace%vdw , mold=neural_network%weights_)
    allocate(workspace%sdw , mold=neural_network%weights_)
    allocate(workspace%vdwc, mold=neural_network%weights_)
    allocate(workspace%sdwc, mold=neural_network%weights_)
    allocate(workspace%dcdb, mold=neural_network%biases_ ) ! Gradient of cost function with respect with biases
    allocate(workspace%vdb , mold=neural_network%biases_ )
    allocate(workspace%sdb , mold=neural_network%biases_ )
    allocate(workspace%vdbc, mold=neural_network%biases_ )
    allocate(workspace%sdbc, mold=neural_network%biases_ )
    allocate(workspace%z    , mold=neural_network%biases_)
    allocate(workspace%delta, mold=neural_network%biases_)

    associate(output_layer => ubound(neural_network%nodes_,1))
      allocate(workspace%a(maxval(neural_network%nodes_), input_layer:output_layer)) ! Activations
    end associate

  end procedure

end submodule 
