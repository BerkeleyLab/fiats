```mermaid

%%{init: { 'theme':'neo',  "class" : {"hideEmptyMembersBox": true} } }%%

classDiagram

    class neural_network_t {
	    neural_network_t(file_t) neural_network_t
	    to_json() file_t
	    infer(inputs : tensor_t) tensor_t
    }
