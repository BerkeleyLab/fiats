submodule(neural_network_m) unmapped_network_s
  implicit none
contains
  module procedure double_precision_unmapped_from_json
    unmapped_network%neural_network_ = double_precision_from_json(file)
  end procedure
end submodule
