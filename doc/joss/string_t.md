Class Diagram
-------------
This Unified Modeling Language (UML) class diagram depicts the derived types and procedures used in example/concurrent-inferences.f90 and in demo/app/train-cloud-microphysics.F90.

```mermaid

%%{init: { 'theme':'neo',  "class" : {"hideEmptyMembersBox": true} } }%%

classDiagram

    class string_t{
      string_t(character~len=*~) string_t
      string() character~len=~)
    }
