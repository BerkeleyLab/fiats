Fiats Neural Network Class Hierarchy
------------------------------------
```mermaid
%%{init: { 'theme':'default',  "class" : {"hideEmptyMembersBox": true} } }%%
classDiagram


neural_network_t    <|-- trainable_network_t
mini_batch_t o-- input_output_pair_t
trainable_network_t -- mini_batch_t

class mini_batch_t{
    + shuffle()
}

class neural_network_t{
    + neural_network_t(file_t) neueral_network_t
    + to_json() file_t
    + map_to_input_range(tensor_t) tensor_t
    + map_from_output_range(tensor_t) tensor_t
    + infer(inputs : tensor_t) tensor_t
    
}
class trainable_network_t{
    + trainable_network_t(file_t) trainable_network_t
    + train(mini_batch_t, cost : real, adam : logical, learnig_rate : real)
    + map_to_training_ranges() input_output_pair_t
}
