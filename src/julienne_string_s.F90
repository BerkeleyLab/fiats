submodule(julienne_string_m) julienne_string_s
  implicit none

  integer, parameter :: default_integer_width_supremum = 11, default_real_width_supremum = 20, double_precision_width_supremum = 25
  integer, parameter :: integer_c_size_t_width_supremum = 19, logical_width=2, comma_width = 1, parenthesis_width = 1, space=1
  
contains

  module procedure as_character
    raw_string = self%string_
  end procedure

  module procedure from_characters
    new_string%string_ = string
  end procedure

  module procedure strings_with_comma_separator
    csv = strings_with_string_t_separator(strings, string_t(","))
  end procedure 

  module procedure characters_with_comma_separator
    csv = strings_with_string_t_separator(string_t(strings), string_t(","))
  end procedure 

  module procedure strings_with_character_separator
    sv = strings_with_string_t_separator(strings, string_t(separator))
  end procedure 

  module procedure strings_with_string_t_separator

    integer s 

    associate(num_elements => size(strings))

      sv = ""

      do s = 1, num_elements - 1
        sv = sv // strings(s) // separator
      end do

      sv = sv // strings(num_elements)

    end associate

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

#ifndef NAGFOR
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
#else
  module procedure get_real
    character(len=:), allocatable :: raw_line, string_value, text_after_colon

    raw_line = self%string()
    text_after_colon = raw_line(index(raw_line, ':')+1:)
    associate(trailing_comma => index(text_after_colon, ','))
      if (trailing_comma == 0) then
        string_value = trim(adjustl((text_after_colon)))
      else
        string_value = trim(adjustl((text_after_colon(:trailing_comma-1))))
      end if
      read(string_value, fmt=*) value_
    end associate

  end procedure
#endif

#ifndef NAGFOR
  module procedure get_double_precision
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
#else
  module procedure get_double_precision
    character(len=:), allocatable :: raw_line, string_value, text_after_colon
    integer trailing_comma

    raw_line = self%string()
    text_after_colon = raw_line(index(raw_line, ':')+1:)
    associate(trailing_comma => index(text_after_colon, ','))
      if (trailing_comma == 0) then
        string_value = trim(adjustl((text_after_colon)))
      else
        string_value = trim(adjustl((text_after_colon(:trailing_comma-1))))
      end if
      read(string_value, fmt=*) value_
    end associate

  end procedure
#endif

  module procedure get_character_with_string_key
    associate(string_value => self%get_string_with_string_key(key, string_t(mold)))
      value_ = string_value%string()
    end associate
  end procedure

  module procedure get_string_t_array_with_string_t_key
    value_ = self%get_string_t_array_with_character_key(key%string(), mold)
  end procedure

  module procedure get_string_t_array_with_character_key

    character(len=:), allocatable :: raw_line
    integer i, comma, opening_quotes, closing_quotes

    raw_line = self%string()

    associate(colon => index(raw_line, ':'))
      associate(opening_bracket => colon + index(raw_line(colon+1:), '['))
        associate(closing_bracket => opening_bracket + index(raw_line(opening_bracket+1:), ']'))
          associate(commas => count([(raw_line(i:i)==",", i = opening_bracket+1, closing_bracket-1)]))
            allocate(value_(commas+1))
            opening_quotes = opening_bracket + index(raw_line(opening_bracket+1:), '"')
            closing_quotes = opening_quotes + index(raw_line(opening_quotes+1:), '"')
            value_(1) = raw_line(opening_quotes+1:closing_quotes-1)
            do i = 1, commas
              comma = closing_quotes + index(raw_line(closing_quotes+1:), ',')
              opening_quotes = comma + index(raw_line(comma+1:), '"')
              closing_quotes = opening_quotes + index(raw_line(opening_quotes+1:), '"')
              value_(i+1) = raw_line(opening_quotes+1:closing_quotes-1)
            end do
          end associate
        end associate
      end associate
    end associate
  end procedure

#ifndef NAGFOR
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
#else
  module procedure get_string_with_string_key

    character(len=:), allocatable :: raw_line, text_after_colon 
    integer opening_value_quotes

    raw_line = self%string()
    text_after_colon = raw_line(index(raw_line, ':')+1:)
    opening_value_quotes = index(text_after_colon, '"')
    associate(closing_value_quotes => opening_value_quotes + index(text_after_colon(opening_value_quotes+1:), '"'))
      if (any([opening_value_quotes, closing_value_quotes] == 0)) then
        value_ = string_t(trim(adjustl((text_after_colon))))
      else
        value_ = string_t(text_after_colon(opening_value_quotes+1:closing_value_quotes-1))
      end if
    end associate

  end procedure
#endif


#ifndef NAGFOR
  module procedure get_logical
    character(len=:), allocatable :: raw_line, string_value

    raw_line = self%string()
    associate(text_after_colon => raw_line(index(raw_line, ':')+1:))
      associate(trailing_comma => index(text_after_colon, ','))
        if (trailing_comma == 0) then
          string_value = trim(adjustl((text_after_colon)))
        else 
          string_value = trim(adjustl((text_after_colon(:trailing_comma-1))))
        end if
        value_ = string_value == "true"
      end associate
    end associate

  end procedure
#else
  module procedure get_logical
    character(len=:), allocatable :: raw_line, string_value, text_after_colon
    integer trailing_comma

    raw_line = self%string()
    text_after_colon = raw_line(index(raw_line, ':')+1:)
    trailing_comma = index(text_after_colon, ',')
    if (trailing_comma == 0) then
      string_value = trim(adjustl((text_after_colon)))
    else 
      string_value = trim(adjustl((text_after_colon(:trailing_comma-1))))
    end if
    value_ = string_value == "true"

  end procedure
#endif

#ifndef NAGFOR
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
#else
  module procedure get_integer
    character(len=:), allocatable :: raw_line, string_value, text_after_colon
    integer trailing_comma 

    raw_line = self%string()
    text_after_colon = raw_line(index(raw_line, ':')+1:)
    trailing_comma = index(text_after_colon, ',')
    if (trailing_comma == 0) then
      string_value = trim(adjustl((text_after_colon)))
    else 
      string_value = trim(adjustl((text_after_colon(:trailing_comma-1))))
    end if
    read(string_value, fmt=*) value_

  end procedure
#endif

  module procedure get_integer_array
    value_ = int(self%get_real_array(key,mold=[0.]))
  end procedure

  module procedure get_real_array
    character(len=:), allocatable :: raw_line
    real, allocatable :: real_array(:)
    integer i

    raw_line = self%string()
    associate(colon => index(raw_line, ":"))
      associate(opening_bracket => colon + index(raw_line(colon+1:), "["))
        associate(closing_bracket => opening_bracket + index(raw_line(opening_bracket+1:), "]"))
          i = 0 ! silence a harmless/bogus warning from gfortran
          associate(commas => count("," == [(raw_line(i:i), i=opening_bracket+1,closing_bracket-1)]))
            associate(num_inputs => commas + 1)
              allocate(real_array(num_inputs))
              read(raw_line(opening_bracket+1:closing_bracket-1), fmt=*) real_array
              value_ = real_array
            end associate
          end associate
        end associate
      end associate
    end associate

  end procedure

  module procedure string_t_eq_string_t
    lhs_eq_rhs = lhs%string() == rhs%string()
  end procedure
   
  module procedure string_t_eq_character
    lhs_eq_rhs = lhs%string() == rhs
  end procedure

  module procedure assign_string_t_to_character
    lhs = rhs%string()
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
   
  module procedure bracket
  
    character(len=:), allocatable :: actual_opening, actual_closing

    associate(opening_present => present(opening))

      if (opening_present) then
        actual_opening = opening
      else
        actual_opening = "["
      end if

      if (present(closing)) then
        actual_closing = closing
      else if(opening_present) then
        actual_closing = actual_opening
      else
        actual_closing = "]"
      end if

    end associate

    bracketed_self = string_t(actual_opening // self%string_ // actual_closing)

  end procedure
   
end submodule julienne_string_s
