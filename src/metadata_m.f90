module metadata_m
  use julienne_string_m, only : string_t
  implicit none
  type metadata_t
    private
    type(string_t) modelName_, modelAuthor_, compilationDate_, activationFunction_, usingSkipConnections_
  end type
end module
