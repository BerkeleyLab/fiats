module julienne_m
  implicit none
  
  type string_t
    character(len=:), allocatable :: string_
  contains
    procedure get_json_value 
  end type

  type file_t
    type(string_t), allocatable :: lines_(:)
  end type

  interface file_t               ! If this generic interface is removed, a
    module procedure from_lines  ! segmentation fault results during or just after
  end interface                  ! the first executable statement in the main program.

contains

  pure function get_json_value(self, key) result(value_)
    class(string_t), intent(in) :: self, key
    real value_
    character(len=:), allocatable :: raw_line, string_value
    raw_line = self%string_
    associate(text_after_colon => raw_line(index(raw_line, ':')+1:))
      associate(trailing_comma => index(text_after_colon, ','))
        if (trailing_comma == 0) then
          string_value = trim(adjustl((text_after_colon)))
        else
          string_value = trim(adjustl((text_after_colon(:trailing_comma-1))))
        end if
        read(string_value, fmt=*) value_
      end associate
    end associate
  end function

  pure function from_lines(lines) result(file_object)
    type(string_t), intent(in) :: lines(:)
    type(file_t) file_object
    file_object%lines_ = lines
  end function

end module

module fiats_m
  use julienne_m, only : string_t, file_t
  implicit none

  type hyperparameters_t(k)
    integer, kind :: k = kind(1.)
    real(k) :: learning_rate_ = real(1.5,k)
  contains
    procedure hyperparameters_to_json
  end type

  interface hyperparameters_t
    module procedure hyperparameters_from_json
  end interface

  character(len=*), parameter :: learning_rate_key = "learning rate"

  type, extends(file_t) :: training_configuration_t(m)
    integer, kind :: m = kind(1.)
    type(hyperparameters_t(m)) hyperparameters_
  contains
    procedure training_configuration_to_json          
  end type

contains

  pure function hyperparameters_from_json(lines) result(hyperparameters)
    type(string_t), intent(in) :: lines(:)
    type(hyperparameters_t) hyperparameters
    hyperparameters%learning_rate_ = lines(1)%get_json_value(string_t(learning_rate_key))
  end function

  pure function hyperparameters_to_json(self) result(lines)
    class(hyperparameters_t), intent(in) :: self
    type(string_t), allocatable :: lines(:)
    integer, parameter :: max_width= 18
    character(len=max_width) learning_rate_string
    write(learning_rate_string,*) self%learning_rate_
    lines = [string_t('"' // learning_rate_key // '" : '  // trim(adjustl(learning_rate_string)) // "," )]
  end function

  pure function training_configuration_from_components(hyperparameters) result(training_configuration)
    type(hyperparameters_t), intent(in) :: hyperparameters
    type(training_configuration_t) training_configuration
    training_configuration%hyperparameters_ = hyperparameters
    training_configuration%file_t = file_t([training_configuration%hyperparameters_%hyperparameters_to_json()])
  end function

  function training_configuration_from_file(lines) result(training_configuration)
    type(string_t), intent(in) :: lines(:)
    type(training_configuration_t) training_configuration
    training_configuration%file_t = file_t(lines)
    training_configuration%hyperparameters_ = hyperparameters_t(training_configuration%file_t%lines_)
  end function

  pure function training_configuration_to_json(self) result(json_lines)
    class(training_configuration_t), intent(in) :: self
    type(string_t), allocatable :: json_lines(:)
    json_lines = self%lines_
  end function

end module

  use fiats_m, only : hyperparameters_t, training_configuration_from_components, training_configuration_from_file, training_configuration_t
  use julienne_m, only : string_t
  implicit none
  type(training_configuration_t) training_configuration, from_json

  training_configuration = training_configuration_from_components(hyperparameters_t(learning_rate_=1.))

  ! Removing the above assignment eliminates the segmentation fault even though the segmentation fault
  ! occurs when executing the assignment below, which does not reference the object defined above.
  ! Alternatively, changing the above line to an `associate` statement gives the compile-time
  ! message: "Error: Invalid kind for REAL at (1)", where the "1" is between `use` and `fiats_m` in
  ! the above use statement.

  print *,new_line(''), "Heading down the rabbit hole..." ! print to demonstrate that execution continues past the above line
  from_json = training_configuration_from_file([string_t('        "learning rate" : 1.00000000,')])
end
