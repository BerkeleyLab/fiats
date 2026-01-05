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

  interface

    elemental module function get_json_key(self) result(unquoted_key)
     implicit none
      class(string_t), intent(in) :: self
      type(string_t) unquoted_key
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

  end interface
  
contains

  module procedure get_json_key
    character(len=:), allocatable :: raw_line
  
    raw_line = self%string_
    associate(opening_key_quotes => index(raw_line, '"'))
      associate(closing_key_quotes => opening_key_quotes + index(raw_line(opening_key_quotes+1:), '"'))
        unquoted_key = string_t(trim(raw_line(opening_key_quotes+1:closing_key_quotes-1)))
      end associate
    end associate

  end procedure

  module procedure get_integer
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

  end procedure

  module procedure string_t_eq_character
    lhs_eq_rhs = lhs%string_ == rhs
  end procedure

end module
