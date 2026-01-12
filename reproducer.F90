module julienne_string_m
  implicit none
  
  private
  public :: string_t

  type string_t
    private
    character(len=:), allocatable :: string_
  contains
    procedure :: as_character
    generic :: string => as_character
    procedure :: get_json_key
    generic :: operator(==)   => string_t_eq_character
    generic :: assignment(= ) => assign_string_t_to_character
    generic :: get_json_value => get_real
    procedure, private :: get_real 
    procedure, private :: string_t_eq_character
    procedure, private, pass(rhs) :: assign_string_t_to_character
  end type

  interface string_t

    elemental module function from_characters(string) result(new_string)
      implicit none
      character(len=*), intent(in) :: string
      type(string_t) new_string
    end function

  end interface

  interface

    pure module function as_character(self) result(raw_string)
      implicit none
      class(string_t), intent(in) :: self
      character(len=:), allocatable :: raw_string
    end function

    elemental module function get_json_key(self) result(unquoted_key)
     implicit none
      class(string_t), intent(in) :: self
      type(string_t) unquoted_key
    end function

    pure module function get_real(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      real, intent(in) :: mold
      real value_
    end function

    elemental module function string_t_eq_character(lhs, rhs) result(lhs_eq_rhs)
      implicit none
      class(string_t), intent(in) :: lhs
      character(len=*), intent(in) :: rhs
      logical lhs_eq_rhs
    end function

    pure module subroutine assign_string_t_to_character(lhs, rhs)
      implicit none
      class(string_t), intent(in) :: rhs
      character(len=:), intent(out), allocatable :: lhs
    end subroutine

  end interface
  
end module julienne_string_m

submodule(julienne_string_m) julienne_string_s
  implicit none

contains

  module procedure as_character
    raw_string = self%string_
  end procedure

  module procedure from_characters
    new_string%string_ = string
  end procedure

  module procedure get_json_key
    character(len=:), allocatable :: raw_line
  
    raw_line = self%string()
    associate(opening_key_quotes => index(raw_line, '"'))
      associate(closing_key_quotes => opening_key_quotes + index(raw_line(opening_key_quotes+1:), '"'))
        unquoted_key = string_t(trim(raw_line(opening_key_quotes+1:closing_key_quotes-1)))
      end associate
    end associate

  end procedure

  module procedure get_real
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

  end procedure

  module procedure string_t_eq_character
    lhs_eq_rhs = lhs%string() == rhs
  end procedure

  module procedure assign_string_t_to_character
    lhs = rhs%string()
  end procedure
   
end submodule julienne_string_s

module julienne_file_m
  use julienne_string_m, only : string_t

  type file_t
    type(string_t), allocatable :: lines_(:)
  end type

  interface file_t

    pure module function from_lines(lines) result(file_object)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(file_t) file_object
    end function

  end interface

end module

submodule(julienne_file_m) julienne_file_s
  implicit none

contains

  module procedure from_lines
    allocate(file_object%lines_, source=lines)
  end procedure

end submodule julienne_file_s

module hyperparameters_m
  use julienne_string_m, only : string_t
  implicit none
  private
  public :: hyperparameters_t

  type hyperparameters_t(k)
    integer, kind :: k = kind(1.)
    real(k), private :: learning_rate_ = real(1.5,k)
  contains
    generic :: to_json => default_real_to_json
    procedure, private :: default_real_to_json
  end type

  interface hyperparameters_t

    pure module function default_real_from_json(lines) result(hyperparameters)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(hyperparameters_t) hyperparameters
    end function

    pure module function default_real_from_components(learning_rate) result(hyperparameters)
      implicit none
      real, intent(in) :: learning_rate
      type(hyperparameters_t) hyperparameters
    end function

  end interface

  interface

    pure module function default_real_to_json(self) result(lines)
      implicit none
      class(hyperparameters_t), intent(in) :: self
      type(string_t), allocatable :: lines(:)
    end function

  end interface

end module

submodule(hyperparameters_m) hyperparameters_s
  implicit none

  character(len=*), parameter :: learning_rate_key = "learning rate"

contains

  module procedure default_real_from_components
    hyperparameters%learning_rate_ = learning_rate
  end procedure 

  module procedure default_real_from_json
    integer l
    logical hyperparameters_key_found 

    hyperparameters_key_found = .false.

    do l=1,size(lines)
      if (lines(l)%get_json_key() == "hyperparameters") then
        hyperparameters_key_found = .true.
        hyperparameters%learning_rate_ = lines(l+1)%get_json_value(string_t(learning_rate_key), mold=0.)
        return
      end if
    end do

  end procedure

  module procedure default_real_to_json
    character(len=*), parameter :: indent = repeat(" ",ncopies=4)
    integer, parameter :: max_width= 18
    character(len=max_width) learning_rate_string

    write(learning_rate_string,*) self%learning_rate_

    lines = [ &
      string_t(indent // '"hyperparameters": {'), &
      string_t(indent // indent // '"' // learning_rate_key // '" : '  // trim(adjustl(learning_rate_string)) // "," ), &
      string_t(indent // '}') &
    ]
  end procedure

end submodule hyperparameters_s

module training_configuration_m
  use julienne_string_m, only : string_t
  use julienne_file_m, only : file_t
  use hyperparameters_m, only : hyperparameters_t
  implicit none

  type, extends(file_t) :: training_configuration_t(m)
    integer, kind :: m = kind(1.)
    type(hyperparameters_t(m)) hyperparameters_
  contains
    generic :: to_json          => default_real_to_json          
    procedure, private          :: default_real_to_json          
  end type
  interface training_configuration_t

    pure module function default_real_from_components(hyperparameters) &
      result(training_configuration)
      implicit none
      type(hyperparameters_t), intent(in) :: hyperparameters
      type(training_configuration_t) training_configuration
    end function

    module function default_real_from_file(file_object) result(training_configuration)
      implicit none
      type(file_t), intent(in) :: file_object
      type(training_configuration_t) training_configuration
    end function

  end interface

  interface

    pure module function default_real_to_json(self) result(json_lines)
      implicit none
      class(training_configuration_t), intent(in) :: self
      type(string_t), allocatable :: json_lines(:)
    end function

  end interface

end module

submodule(training_configuration_m) training_configuration_s
  implicit none

contains

  module procedure default_real_from_components

    training_configuration%hyperparameters_ = hyperparameters

    training_configuration%file_t = file_t([ &
      string_t("{"), &
      training_configuration%hyperparameters_%to_json(), &
      string_t("}") &
    ])

  end procedure

  module procedure default_real_from_file
    training_configuration%file_t = file_object

    associate(lines => training_configuration%file_t%lines_)
      training_configuration%hyperparameters_ = hyperparameters_t(lines)
    end associate
  end procedure

  module procedure default_real_to_json
    json_lines = self%lines_
  end procedure

end submodule training_configuration_s

program test_suite_driver
  use training_configuration_m, only : training_configuration_t
  use hyperparameters_m, only : hyperparameters_t
  use julienne_file_m, only : file_t
  use julienne_string_m, only : string_t
  implicit none

  type(training_configuration_t) training_configuration 

  training_configuration = training_configuration_t(hyperparameters_t(learning_rate=1.))
  ! Removing the above assignment eliminates the segmentation fault even though the segmentation fault occurs
  ! when executing the assignment below, which does not reference the object define above.

  print *,new_line(''), "Heading down the rabbit hole..." ! print to demonstrate that execution continues past the above line

  block
    type(training_configuration_t) from_json
    from_json = training_configuration_t(file_t( &
       [ string_t('{') &
        ,string_t('    "hyperparameters": {') &
        ,string_t('        "learning rate" : 1.00000000,') &
        ,string_t('    }') &
        ,string_t('}') &
       ] &
    ))
  end block

end program
