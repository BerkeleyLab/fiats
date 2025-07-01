
```mermaid

%%{init: { 'theme':'neo',  "class" : {"hideEmptyMembersBox": true} } }%%

classDiagram

    class trainable_network_t {
	    trainable_network_t(file_t) trainable_network_t
	    trainable_network_t(training_configuration_t, real, metadata, tensor_map_t, tensor_map_t) trainable_network_t
	    train(mini_batch_t, cost : real, adam : logical, learnig_rate : real)
    }
