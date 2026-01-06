module julienne_string_m
  implicit none
  
  type string_t
    character(len=:), allocatable :: string_
  contains
    procedure :: get_json_key
    generic :: operator(==)   => string_t_eq_character
    generic :: get_json_value => get_integer
    procedure get_integer
    procedure string_t_eq_character
  end type

contains

  elemental function get_json_key(self) result(unquoted_key)
    class(string_t), intent(in) :: self
    type(string_t) unquoted_key
    character(len=:), allocatable :: raw_line
  
    raw_line = self%string_
    associate(opening_key_quotes => index(raw_line, '"'))
      associate(closing_key_quotes => opening_key_quotes + index(raw_line(opening_key_quotes+1:), '"'))
        unquoted_key = string_t(trim(raw_line(opening_key_quotes+1:closing_key_quotes-1)))
      end associate
    end associate
  end function

  pure function get_integer(self, key, mold) result(value_)
    class(string_t), intent(in) :: self, key
    integer, intent(in) ::  mold
    integer value_
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

  elemental function string_t_eq_character(lhs, rhs) result(lhs_eq_rhs)
    class(string_t), intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    logical lhs_eq_rhs
    lhs_eq_rhs = lhs%string_ == rhs
  end function

end module
module julienne_file_m
  use julienne_string_m, only : string_t

  type file_t
    type(string_t), allocatable :: lines_(:)
  end type

  interface file_t
    module procedure from_lines
  end interface

contains

  type(file_t) pure function from_lines(lines)
    type(string_t), intent(in) :: lines(:)
    allocate(from_lines%lines_, source=lines) ! switching this to an assignment (from_lines%lines_ = lines) prevents the seg fault
  end function

end module
module hyperparameters_m
  use julienne_string_m, only : string_t
  implicit none

  type hyperparameters_t(k)
    integer, kind :: k = kind(1.)
    integer :: mini_batches_ = 10
  contains
    procedure to_json
  end type

  interface hyperparameters_t
    module procedure default_real_from_json
  end interface

  character(len=*), parameter :: mini_batches_key  = "mini-batches"

contains

  pure function default_real_from_json(lines) result(hyperparameters)
    type(string_t), intent(in) :: lines(:)
    type(hyperparameters_t) hyperparameters
    integer l

    do l=1,size(lines)
      if (lines(l)%get_json_key() == "hyperparameters") then
        hyperparameters%mini_batches_  = lines(l+1)%get_json_value(string_t(mini_batches_key), mold=0)
        return
      end if
    end do
  end function

  pure function to_json(self) result(lines)
    class(hyperparameters_t), intent(in) :: self
    type(string_t), allocatable :: lines(:)
    integer, parameter :: max_width= 18
    character(len=max_width) mini_batches_string

    write(mini_batches_string,*) self%mini_batches_
    lines = [string_t('"hyperparameters": {"' // mini_batches_key  // '" : '  // trim(adjustl(mini_batches_string)) // '}')]
  end function

end module
module training_configuration_m
  use julienne_file_m, only : file_t
  use julienne_string_m, only : string_t
  use hyperparameters_m, only : hyperparameters_t
  implicit none

  type, extends(file_t) :: training_configuration_t(m)
    integer, kind :: m = kind(1.)
    type(hyperparameters_t(m)) hyperparameters_
  end type

  interface training_configuration_t
    module procedure default_real_from_components
    module procedure default_real_from_file
  end interface

contains

  pure function default_real_from_components(hyperparameters) result(training_configuration)
    type(hyperparameters_t), intent(in) :: hyperparameters
    type(training_configuration_t) training_configuration
    training_configuration%hyperparameters_ = hyperparameters
    training_configuration%file_t = file_t([ string_t("{"), training_configuration%hyperparameters_%to_json(), string_t("}") ])
  end function

  function default_real_from_file(file_object) result(training_configuration)
    type(file_t), intent(in) :: file_object
    type(training_configuration_t) training_configuration
    training_configuration%file_t = file_object
    training_configuration%hyperparameters_ = hyperparameters_t(training_configuration%file_t%lines_)
  end function

end module
program test_suite_driver
  use training_configuration_m, only : training_configuration_t
  use hyperparameters_m, only : hyperparameters_t
  use julienne_file_m, only : file_t
  implicit none

  type(training_configuration_t) training_configuration 

  training_configuration = training_configuration_t(hyperparameters_t())

  block
    type(training_configuration_t) from_json
    from_json = training_configuration_t(file_t(training_configuration%file_t%lines_))
  end block

end program
