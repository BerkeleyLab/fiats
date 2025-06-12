Class Diagram
-------------
This Unified Modeling Language (UML) class diagram depicts the derived types and procedures used in example/concurrent-inferences.f90 and in demo/app/train-cloud-microphysics.F90.

```mermaid

%%{init: { 'theme':'neo',  "class" : {"hideEmptyMembersBox": true} } }%%

classDiagram

    class trainable_network_t {
	    + trainable_network_t(file_t) trainable_network_t
	    + trainable_network_t(training_configuration_t, real, metadata, tensor_map_t, tensor_map_t) trainable_network_t
	    + train(mini_batch_t, cost : real, adam : logical, learnig_rate : real)
    }

    class mini_batch_t {
      + mini_batch_t(input_output_pairs(:) : input_output_pair_t)
	    + shuffle()
    }

    class input_output_pair_t {
      input_output_pair_t(inputs(:) : real, outputs(:) : real) input_output_pair_t
    }

    class neural_network_t {
	    + neural_network_t(file_t) neural_network_t
	    + to_json() file_t
	    + infer(inputs : tensor_t) tensor_t
    }

    class file_t{
      file_t(string_t) file_t
    }

    class double_precision_file_t{
      double_precision_file_t(string_t) double_precision_file_t
    }

    class string_t{
      string_t(character~len=*~) string_t
      string() character~len=~)
    }

    class tensor_t~kind~{
      tensor_t(values : real) tensor_t
      tensor_t(values : real~kind~) tensor_t
    }

    class tensor_map_t{
      tensor_map_t(layer : character~len=*~, minima(:) : real, maxima(:) : real) : tensor_map_t
    }

    class unmapped_network_t~kind~{
      unmapped_network_t(double_precision_file_t) unmapped_network_t~kind~
    }

    class training_configuration_t{
      training_configuration_t(file_t) training_configuration_t
    }

    file_t *-- string_t : has an array of
    file_t <|-- double_precision_file_t : extends
    mini_batch_t *-- input_output_pair_t : has an array of
    neural_network_t <|-- trainable_network_t : extends
    neural_network_t --> file_t : accepts as user-defined structure constructor argument
    neural_network_t --> tensor_t : accepts as infer() argument and produces as infer() result
    trainable_network_t --> training_configuration_t : accepts as user-defined structure constructor argument
    trainable_network_t --> mini_batch_t : accepts as train() argument
    trainable_network_t --> tensor_map_t : accepts as a user-defined structure constructor argument
    trainable_network_t --> file_t : accepts as user-defined structure constructor argument
    unmapped_network_t *-- neural_network_t : has an
    unmapped_network_t --> double_precision_file_t : accepts as user-defined structure constructor argument
