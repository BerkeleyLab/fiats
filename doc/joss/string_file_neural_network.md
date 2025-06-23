```mermaid

%%{init: { 'theme':'neo',  "class" : {"hideEmptyMembersBox": true} } }%%

classDiagram

    class string_t{
      string_t(character~len=*~) string_t
      string() character~len=~)
    }

    class file_t{
      file_t(string_t) file_t
   }

    class neural_network_t {
	    + neural_network_t(file_t) neural_network_t
	    + to_json() file_t
	    + infer(inputs : tensor_t) tensor_t
    }
