Fiats Neural Network Class Hierarchy
------------------------------------
```mermaid
classDiagram

neural_network_t    <|-- trainable_network_t
neural_network_t     o-- metadata_t
neural_network_t     o-- tensor_map_t
neural_network_t     o-- activation_t
trainable_network_t  o-- workspace_t

class neural_network_t{
    - input_map_ : tensor_map_t
    - output_map_ : tensor_map_t
    - activation_ : activation_t
    - metadata_ : metadata_t
    - weights_ : real
    - bisases_ : real
    - nodes_ : integer
    + operator(==) logical
    + infer(tensor_t) tensor_t
    + to_json() file_t
    + map_to_input_range(tensor_t) tensor_t
    + map_from_output_range(tensor_t) tensor_t
    + num_hidden_layers() integer
    + num_inputs() integer
    + num_outputs() integer
    + nodes_per_layer() integer
    + assert_conformable_with()
    + skip() logical
    + activation_function_name() string_t
    + learn(mini_batch_t, real, logical, real, workspace_t)
    + assert_consistency()
}
class trainable_network_t{
    - workspace_ : workspace_t
    + train(mini_batch_t, real, logical, real)
    + map_to_training_ranges() : input_output_pair_t
}
class tensor_map_t{
  - layer_ : character
  - intercept_ : real
  - slope_ : real
  + to_json() : file_t
  + operator(==) logical
}
class tensor_t{
  - values_ : real
  + values() real
  - num_components() integer
}
