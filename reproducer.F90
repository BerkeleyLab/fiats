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
    generic :: get_json_value => get_string_with_string_key, get_real, get_integer
    procedure, private :: get_string_with_string_key
    procedure, private :: get_real 
    procedure, private :: get_integer 
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

    pure module function get_string_with_string_key(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key, mold
      type(string_t) :: value_
    end function

    pure module function get_integer(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      integer, intent(in) ::  mold
      integer value_
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

  module procedure get_string_with_string_key

    character(len=:), allocatable :: raw_line

    raw_line = self%string()
    associate(text_after_colon => raw_line(index(raw_line, ':')+1:))
      associate(opening_value_quotes => index(text_after_colon, '"'))
        associate(closing_value_quotes => opening_value_quotes + index(text_after_colon(opening_value_quotes+1:), '"'))
          if (any([opening_value_quotes, closing_value_quotes] == 0)) then
            value_ = string_t(trim(adjustl((text_after_colon))))
          else
            value_ = string_t(text_after_colon(opening_value_quotes+1:closing_value_quotes-1))
          end if
        end associate
      end associate
    end associate

  end procedure

  module procedure get_integer
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

    module function from_file_with_string_name(file_name) result(file_object)
      implicit none
      type(string_t), intent(in) :: file_name
      type(file_t) file_object
    end function

    module function from_file_with_character_name(file_name) result(file_object)
      implicit none
      character(len=*), intent(in) :: file_name
      type(file_t) file_object
    end function

    pure module function from_lines(lines) result(file_object)
      implicit none
      type(string_t), intent(in) :: lines(:)
      type(file_t) file_object
    end function

  end interface

end module

submodule(julienne_file_m) julienne_file_s
  use iso_fortran_env, only : iostat_end, iostat_eor, output_unit
  implicit none

contains

  module procedure from_lines
    allocate(file_object%lines_, source=lines)
  end procedure

  module procedure from_file_with_character_name
    file_object = from_file_with_string_name(string_t(file_name))
  end procedure

  module procedure from_file_with_string_name

    integer io_status, file_unit, line_num
    character(len=:), allocatable :: line
    integer, parameter :: max_message_length=128
    character(len=max_message_length) error_message
    integer, allocatable :: lengths(:)

    open(newunit=file_unit, file=file_name%string(), form='formatted', status='old')

    lengths = line_lengths(file_unit)

    associate(num_lines => size(lengths))

      allocate(file_object%lines_(num_lines))
  
      do line_num = 1, num_lines
        allocate(character(len=lengths(line_num)) :: line)
        read(file_unit, '(a)') line
        file_object%lines_(line_num) = string_t(line)
        deallocate(line)
      end do

    end associate

    close(file_unit)

  contains
   
    function line_count(file_unit) result(num_lines)
      integer, intent(in) :: file_unit
      integer num_lines
    
      rewind(file_unit)
      num_lines = 0 
      do  
        read(file_unit, *, iostat=io_status)
        if (io_status==iostat_end) exit
        num_lines = num_lines + 1 
      end do
      rewind(file_unit)
    end function

    function line_lengths(file_unit) result(lengths)
      integer, intent(in) :: file_unit
      integer, allocatable ::  lengths(:)
      integer io_status, l
      character(len=1) c

      associate(num_lines => line_count(file_unit))

        allocate(lengths(num_lines), source = 0)
        rewind(file_unit)

        do l = 1, num_lines
          do
            read(file_unit, '(a)', advance='no', iostat=io_status, iomsg=error_message) c
            associate(eliminate_unused_variable_warning => c) ! eliminate NAG compiler "variable c set but never referenced" warning
            end associate
            if (io_status==iostat_eor .or. io_status==iostat_end) exit
            lengths(l) = lengths(l) + 1
          end do
        end do

        rewind(file_unit)
  
      end associate
    end function

  end procedure

end submodule julienne_file_s

module hyperparameters_m
  use julienne_string_m, only : string_t
  implicit none
  private
  public :: hyperparameters_t

  type hyperparameters_t(k)
    integer, kind :: k = kind(1.)
    integer, private:: mini_batches_ = 10
    real(k), private :: learning_rate_ = real(1.5,k)
    character(len=:), allocatable :: optimizer_
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

    pure module function default_real_from_components(mini_batches, learning_rate, optimizer) result(hyperparameters)
      implicit none
      integer, intent(in) :: mini_batches
      real, intent(in) :: learning_rate
      character(len=*), intent(in) :: optimizer
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

  character(len=*), parameter :: mini_batches_key  = "mini-batches"
  character(len=*), parameter :: learning_rate_key = "learning rate"
  character(len=*), parameter :: optimizer_key     = "optimizer"

contains

  module procedure default_real_from_components
    hyperparameters%mini_batches_ = mini_batches
    hyperparameters%learning_rate_ = learning_rate
    hyperparameters%optimizer_ = optimizer
  end procedure 

  module procedure default_real_from_json
    integer l
    logical hyperparameters_key_found 

    hyperparameters_key_found = .false.

    do l=1,size(lines)
      if (lines(l)%get_json_key() == "hyperparameters") then
        hyperparameters_key_found = .true.
        hyperparameters%mini_batches_  = lines(l+1)%get_json_value(string_t(mini_batches_key), mold=0)
        hyperparameters%learning_rate_ = lines(l+2)%get_json_value(string_t(learning_rate_key), mold=0.)
        hyperparameters%optimizer_ = lines(l+3)%get_json_value(string_t(optimizer_key), mold=string_t(""))
        return
      end if
    end do

  end procedure

  module procedure default_real_to_json
    character(len=*), parameter :: indent = repeat(" ",ncopies=4)
    integer, parameter :: max_width= 18
    character(len=max_width) mini_batches_string, learning_rate_string

    write(mini_batches_string,*) self%mini_batches_
    write(learning_rate_string,*) self%learning_rate_

    lines = [ &
      string_t(indent // '"hyperparameters": {'), &
      string_t(indent // indent // '"' // mini_batches_key  // '" : '  // trim(adjustl(mini_batches_string))  // "," ), &
      string_t(indent // indent // '"' // learning_rate_key // '" : '  // trim(adjustl(learning_rate_string)) // "," ), &
      string_t(indent // indent // '"' // optimizer_key     // '" : "' // trim(adjustl(self%optimizer_     )) // '"'), &
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

  character(len=*), parameter :: header="{", footer="}", separator = ","

contains

  module procedure default_real_from_components

    training_configuration%hyperparameters_ = hyperparameters

    training_configuration%file_t = file_t([ &
      string_t(header), &
      training_configuration%hyperparameters_%to_json(), &
      string_t(footer) &
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

  training_configuration = training_configuration_t( hyperparameters_t(mini_batches=5, learning_rate=1., optimizer = "adam"))
  ! Removing the above assignment eliminates the segmentation fault even though the segmentation fault occurs
  ! when executing the assignment below, which does not reference the object define above.

  print *,new_line(''), "Heading down the rabbit hole..." ! print to demonstrate that execution continues past the above line

  block
    type(training_configuration_t) from_json
    from_json = training_configuration_t(file_t( &
       [ string_t('{') &
        ,string_t('    "hyperparameters": {') &
        ,string_t('        "mini-batches" : 5,') &
        ,string_t('        "learning rate" : 1.00000000,') &
        ,string_t('        "optimizer" : "adam"') &
        ,string_t('    }') &
        ,string_t('}') &
       ] &
    ))
  end block

end program
