module julienne_m
  implicit none
  
  private
  public string_t
  public file_t

  type string_t
    private
    character(len=:), allocatable :: string_
  contains
    procedure string 
    procedure get_json_key
    procedure get_json_value 
    generic :: operator(==)   => string_t_eq_character
    generic :: assignment(= ) => assign_string_t_to_character
    procedure, private :: string_t_eq_character
    procedure, private, pass(rhs) :: assign_string_t_to_character
  end type

  interface string_t
    module procedure from_characters
  end interface

  type file_t
    type(string_t), allocatable :: lines_(:)
  end type

  interface file_t               ! If this generic interface
    module procedure from_lines  ! is removed, the program
  end interface                  ! seg faults sooner.

contains

  pure function string(self) result(raw_string)
    class(string_t), intent(in) :: self
    character(len=:), allocatable :: raw_string
    raw_string = self%string_
  end function

  elemental function from_characters(string) result(new_string)
    character(len=*), intent(in) :: string
    type(string_t) new_string
    new_string%string_ = string
  end function

  elemental function get_json_key(self) result(unquoted_key)
    class(string_t), intent(in) :: self
    type(string_t) unquoted_key
    character(len=:), allocatable :: raw_line
    
    raw_line = self%string()
    associate(opening_key_quotes => index(raw_line, '"'))
      associate(closing_key_quotes => opening_key_quotes + index(raw_line(opening_key_quotes+1:), '"'))
        unquoted_key = string_t(trim(raw_line(opening_key_quotes+1:closing_key_quotes-1)))
      end associate
    end associate
  end function

  pure function get_json_value(self, key, mold) result(value_)
    class(string_t), intent(in) :: self, key
    real, intent(in) :: mold
    real value_
    character(len=:), allocatable :: raw_line, string_value

    raw_line = self%string()
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

  elemental function string_t_eq_character(lhs, rhs) result(lhs_eq_rhs)
    class(string_t), intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    logical lhs_eq_rhs
    lhs_eq_rhs = lhs%string() == rhs
  end function

  pure subroutine assign_string_t_to_character(lhs, rhs)
    class(string_t), intent(in) :: rhs
    character(len=:), intent(out), allocatable :: lhs
    lhs = rhs%string()
  end subroutine
   
  pure function from_lines(lines) result(file_object)
    type(string_t), intent(in) :: lines(:)
    type(file_t) file_object
    allocate(file_object%lines_, source=lines)
  end function

end module

module hyperparameters_m
  use julienne_m, only : string_t
  implicit none
  private
  public :: hyperparameters_t

  type hyperparameters_t(k)
    integer, kind :: k = kind(1.)
    real(k), private :: learning_rate_ = real(1.5,k)
  contains
    procedure to_json
  end type

  interface hyperparameters_t
    module procedure hyperparameters_from_json, hyperparameters_from_components
  end interface

  character(len=*), parameter :: learning_rate_key = "learning rate"

contains

  pure function hyperparameters_from_components(learning_rate) result(hyperparameters)
    real, intent(in) :: learning_rate
    type(hyperparameters_t) hyperparameters
    hyperparameters%learning_rate_ = learning_rate
  end function

  pure function hyperparameters_from_json(lines) result(hyperparameters)
    type(string_t), intent(in) :: lines(:)
    type(hyperparameters_t) hyperparameters
    hyperparameters%learning_rate_ = lines(1)%get_json_value(string_t(learning_rate_key), mold=0.)
  end function

  pure function to_json(self) result(lines)
    class(hyperparameters_t), intent(in) :: self
    type(string_t), allocatable :: lines(:)
    character(len=*), parameter :: indent = repeat(" ",ncopies=4)
    integer, parameter :: max_width= 18
    character(len=max_width) learning_rate_string

    write(learning_rate_string,*) self%learning_rate_
    lines = [string_t(indent // indent // '"' // learning_rate_key // '" : '  // trim(adjustl(learning_rate_string)) // "," )]
  end function

end module

module training_configuration_m
  use julienne_m, only : string_t, file_t
  use hyperparameters_m, only : hyperparameters_t
  implicit none

  type, extends(file_t) :: training_configuration_t(m)
    integer, kind :: m = kind(1.)
    type(hyperparameters_t(m)) hyperparameters_
  contains
    procedure to_json          
  end type

  interface training_configuration_t
    module procedure training_configuration_from_components, training_configuration_from_file
  end interface

contains

  pure function training_configuration_from_components(hyperparameters) result(training_configuration)
    type(hyperparameters_t), intent(in) :: hyperparameters
    type(training_configuration_t) training_configuration

    training_configuration%hyperparameters_ = hyperparameters
    training_configuration%file_t = file_t([training_configuration%hyperparameters_%to_json()])
  end function

  function training_configuration_from_file(lines) result(training_configuration)
    type(string_t), intent(in) :: lines(:)
    type(training_configuration_t) training_configuration

    training_configuration%file_t = file_t(lines)
    training_configuration%hyperparameters_ = hyperparameters_t(training_configuration%file_t%lines_)
  end function

  pure function to_json(self) result(json_lines)
    class(training_configuration_t), intent(in) :: self
    type(string_t), allocatable :: json_lines(:)
    json_lines = self%lines_
  end function

end module

program test_suite_driver
  use training_configuration_m, only : training_configuration_t
  use hyperparameters_m, only : hyperparameters_t
  use julienne_m, only : string_t
  implicit none

  type(training_configuration_t) training_configuration 

  training_configuration = training_configuration_t(hyperparameters_t(learning_rate=1.))
  ! Removing the above assignment eliminates the segmentation fault even though the segmentation fault occurs
  ! when executing the assignment below, which does not reference the object define above.

  print *,new_line(''), "Heading down the rabbit hole..." ! print to demonstrate that execution continues past the above line

  block
    type(training_configuration_t) from_json
    from_json = training_configuration_t([string_t('        "learning rate" : 1.00000000,')])
  end block

end program
