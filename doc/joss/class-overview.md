Class Diagram
-------------
This Unified Modeling Language (UML) class diagram depicts the derived types and procedures used in example/concurrent-inferences.f90 and in demo/app/train-cloud-microphysics.F90.

```mermaid

%%{init: { 'theme':'neo',  "class" : {"hideEmptyMembersBox": true} } }%%

classDiagram

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
