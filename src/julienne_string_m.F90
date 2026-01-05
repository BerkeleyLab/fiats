module julienne_string_m
  use iso_c_binding, only : c_bool, c_size_t
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
    generic :: operator(//)   => string_t_cat_string_t, string_t_cat_character, character_cat_string_t
    generic :: operator(==)   => string_t_eq_string_t, string_t_eq_character
    generic :: assignment(= ) => assign_character_to_string_t
    generic :: get_json_value => get_string_with_string_key &
                                ,get_integer
    procedure, private :: get_string_with_string_key
    procedure, private :: get_integer 
    procedure, private :: string_t_eq_string_t, string_t_eq_character
    procedure, private :: assign_character_to_string_t
    procedure, private :: string_t_cat_string_t, string_t_cat_character
    procedure, private, pass(rhs) :: character_cat_string_t
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

    elemental module function string_t_eq_string_t(lhs, rhs) result(lhs_eq_rhs)
      implicit none
      class(string_t), intent(in) :: lhs, rhs
      logical lhs_eq_rhs
    end function

    elemental module function string_t_eq_character(lhs, rhs) result(lhs_eq_rhs)
      implicit none
      class(string_t), intent(in) :: lhs
      character(len=*), intent(in) :: rhs
      logical lhs_eq_rhs
    end function

    elemental module function string_t_cat_string_t(lhs, rhs) result(lhs_cat_rhs)
      implicit none
      class(string_t), intent(in) :: lhs, rhs
      type(string_t) lhs_cat_rhs
    end function

    elemental module function string_t_cat_character(lhs, rhs) result(lhs_cat_rhs)
      implicit none
      class(string_t), intent(in) :: lhs
      character(len=*), intent(in) :: rhs
      type(string_t) lhs_cat_rhs
    end function

    elemental module function character_cat_string_t(lhs, rhs) result(lhs_cat_rhs)
      implicit none
      character(len=*), intent(in) :: lhs
      class(string_t), intent(in) :: rhs
      type(string_t) lhs_cat_rhs
    end function

    elemental module subroutine assign_character_to_string_t(lhs, rhs)
      implicit none
      class(string_t), intent(inout) :: lhs
      character(len=*), intent(in) :: rhs
    end subroutine

  end interface
  
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

  module procedure string_t_eq_string_t
    lhs_eq_rhs = lhs%string() == rhs%string()
  end procedure
   
  module procedure string_t_eq_character
    lhs_eq_rhs = lhs%string() == rhs
  end procedure

  module procedure assign_character_to_string_t
    lhs%string_ = rhs
  end procedure

  module procedure string_t_cat_string_t
    lhs_cat_rhs = string_t(lhs%string_ // rhs%string_)
  end procedure
   
  module procedure string_t_cat_character
    lhs_cat_rhs = string_t(lhs%string_ // rhs)
  end procedure

  module procedure character_cat_string_t
    lhs_cat_rhs = string_t(lhs // rhs%string_)
  end procedure
   
end module julienne_string_m
