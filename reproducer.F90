module julienne_fiats_m
  implicit none
  
  type string_t
    character(len=:), allocatable :: string_
  end type

  type file_t
    type(string_t), allocatable :: lines_(:)
  end type

  type hyperparameters_t(k) ! making this and the type below non-PDTs eliminates the segmentation fault
    integer, kind :: k = kind(1.)
    real(k) :: learning_rate_ = real(1.5,k)
  end type

  type, extends(file_t) :: training_configuration_t(m) ! making this a non-PDT gives a compile-time error
    integer, kind :: m = kind(1.)
    type(hyperparameters_t(m)) hyperparameters_
  end type

contains

  real function get_json_value(string)
    character(len=*) string
    read(string, fmt=*) get_json_value
    print *,"expecting 1.000000, read ", get_json_value
  end function

  function hyperparameters_to_json(self) result(lines)! invoked only by training_config_from_component
    type(hyperparameters_t) self
    type(string_t), allocatable :: lines(:)
    integer, parameter :: max_width= 18
    character(len=max_width) learning_rate_string
    write(learning_rate_string,*) self%learning_rate_
    lines = [string_t(learning_rate_string)]
  end function

  type(training_configuration_t) function training_config_from_component(hyperparameters)
    type(hyperparameters_t) hyperparameters
    training_config_from_component%hyperparameters_ = hyperparameters
    training_config_from_component%file_t = file_t([hyperparameters_to_json(training_config_from_component%hyperparameters_)])
  end function

  function hyperparameters_from_json(lines) result(hyperparameters) ! invoked only by training_config_from_file
    type(string_t) lines(:)
    type(hyperparameters_t) hyperparameters
    hyperparameters%learning_rate_ = get_json_value(lines(1)%string_)
  end function

  type(training_configuration_t) function training_config_from_file(line)
    character(len=*) line
    training_config_from_file%file_t = file_t([string_t(line)])
    training_config_from_file%hyperparameters_ = hyperparameters_from_json(training_config_from_file%file_t%lines_)
  end function

end module

  use julienne_fiats_m
  implicit none
  type(training_configuration_t) training_configuration, from_json

  training_configuration = training_config_from_component(hyperparameters_t(learning_rate_=1.))

  ! Removing the above assignment eliminates the segmentation fault even though the segmentation fault
  ! occurs when executing the assignment below, which does not reference the object defined above.
  ! Alternatively, changing the above line to an `associate` statement gives the compile-time
  ! message: "Error: Invalid kind for REAL at (1)", where the "1" is between `use` and `fiats_m` in
  ! the above use statement.

  print *,new_line(''), "Heading down the rabbit hole..." ! print to demonstrate that execution continues past the above line
  from_json = training_config_from_file('1.00000000')
end
