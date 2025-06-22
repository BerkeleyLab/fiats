Class Diagram
-------------
This Unified Modeling Language (UML) class diagram depicts the derived types and procedures used in example/concurrent-inferences.f90 and in demo/app/train-cloud-microphysics.F90.

```mermaid

%%{init: { 'theme':'neo',  "class" : {"hideEmptyMembersBox": true} } }%%

classDiagram

    class tensor_map_t{
      tensor_map_t(layer : character~len=*~, minima(:) : real, maxima(:) : real) : tensor_map_t
    }
