! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module julienne_string_m
  use characterizable_m, only : characterizable_t
  implicit none
  
  type, extends(characterizable_t) :: string_t
    character(len=:), allocatable :: string_
  contains
    procedure :: as_character
    generic :: string => as_character
    generic :: assignment(= ) => assign_string_t_to_character, assign_character_to_string_t
    procedure, private :: assign_character_to_string_t
    procedure, private, pass(rhs) :: assign_string_t_to_character
  end type

  interface string_t

    elemental module function construct(string) result(new_string)
      implicit none
      character(len=*), intent(in) :: string
      type(string_t) new_string
    end function

    elemental module function from_default_integer(i) result(string)
      implicit none
      integer, intent(in) :: i
      type(string_t) string
    end function

    elemental module function from_real(x) result(string)
      implicit none
      real, intent(in) :: x
      type(string_t) string
    end function

  end interface

  interface operator(.cat.)

   pure  module function concatenate_elements(strings) result(concatenated_strings)
      implicit none
      type(string_t), intent(in) :: strings(:)
      type(string_t) concatenated_strings
    end function

  end interface

  interface operator(.csv.)

    pure module function strings_with_comma_separator(strings) result(csv)
      implicit none
      type(string_t), intent(in) :: strings(:)
      type(string_t) csv
    end function

    pure module function characters_with_comma_separator(strings) result(csv)
      implicit none
      character(len=*), intent(in) :: strings(:)
      type(string_t) csv
    end function

  end interface

  interface operator(.sv.)

    pure module function strings_with_character_separator(strings, separator) result(sv)
      implicit none
      type(string_t)  , intent(in) :: strings(:)
      character(len=*), intent(in) :: separator
      type(string_t) sv
    end function

    pure module function characters_with_character_separator(strings, separator) result(sv)
      implicit none
      character(len=*), intent(in) :: strings(:), separator
      type(string_t) sv
    end function

    pure module function characters_with_string_separator(strings, separator) result(sv)
      implicit none
      character(len=*), intent(in) :: strings(:)
      type(string_t)  , intent(in) :: separator
      type(string_t) sv
    end function

    pure module function strings_with_string_t_separator(strings, separator) result(sv)
      implicit none
      type(string_t), intent(in) :: strings(:), separator
      type(string_t) sv 
    end function

  end interface

  interface

    pure module function as_character(self) result(raw_string)
      implicit none
      class(string_t), intent(in) :: self
      character(len=:), allocatable :: raw_string
    end function

    pure module function array_of_strings(delimited_strings, delimiter) result(strings_array)
      implicit none
      character(len=*), intent(in) :: delimited_strings, delimiter
      type(string_t), allocatable :: strings_array(:)
    end function

    elemental module function is_allocated(self) result(string_allocated)
      implicit none
      class(string_t), intent(in) :: self
      logical string_allocated
    end function

    elemental module function get_json_key(self) result(unquoted_key)
     implicit none
      class(string_t), intent(in) :: self
      type(string_t) unquoted_key
    end function

    elemental module function file_extension(self) result(extension)
      !! result contains all characters in file_name after the last dot (.)
      class(string_t), intent(in) :: self
      type(string_t) extension
    end function

    pure module function base_name(self) result(base)
      !! result contains all characters in file_name before the last dot (.)
      class(string_t), intent(in) :: self
      type(string_t) base
    end function

    pure module function get_real(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      real, intent(in) :: mold
      real value_
    end function

    pure module function get_real_with_character_key(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self
      character(len=*), intent(in) :: key
      real, intent(in) :: mold
      real value_
    end function

    pure module function get_double_precision(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      double precision, intent(in) :: mold
      double precision value_
    end function

    pure module function get_double_precision_with_character_key(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self
      character(len=*), intent(in) :: key
      double precision, intent(in) :: mold
      double precision value_
    end function

    pure module function get_double_precision_array(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      double precision, intent(in) :: mold(:)
      double precision, allocatable :: value_(:)
    end function

    pure module function get_double_precision_array_with_character_key(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self
      character(len=*), intent(in) :: key
      double precision, intent(in) :: mold(:)
      double precision, allocatable :: value_(:)
    end function

    pure module function get_character(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      character(len=*), intent(in) :: mold
      character(len=:), allocatable :: value_
    end function

    pure module function get_character_with_character_key(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self
      character(len=*), intent(in) :: key, mold
      character(len=:), allocatable :: value_
    end function

    elemental module function get_string(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key, mold
      type(string_t) :: value_
    end function

    pure module function get_string_t_array(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      type(string_t), intent(in) :: mold(:)
      type(string_t), allocatable :: value_(:)
    end function

    pure module function get_integer_with_character_key(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self
      character(len=*), intent(in) :: key
      integer, intent(in) ::  mold
      integer value_
    end function

    pure module function get_integer(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      integer, intent(in) ::  mold
      integer value_
    end function

    pure module function get_logical_with_character_key(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self
      character(len=*), intent(in) :: key
      logical, intent(in) :: mold
      logical value_
    end function

    elemental module function get_logical(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      logical, intent(in) :: mold
      logical value_
    end function

    pure module function get_integer_array_with_character_key(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self
      character(len=*), intent(in) :: key
      integer, intent(in) :: mold(:)
      integer, allocatable :: value_(:)
    end function

    pure module function get_integer_array(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      integer, intent(in) :: mold(:)
      integer, allocatable :: value_(:)
    end function

    pure module function get_real_array_with_character_key(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self
      character(len=*), intent(in) :: key
      real, intent(in) :: mold(:)
      real, allocatable :: value_(:)
    end function

    pure module function get_real_array(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      real, intent(in) :: mold(:)
      real, allocatable :: value_(:)
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

    elemental module function character_eq_string_t(lhs, rhs) result(lhs_eq_rhs)
      implicit none
      class(string_t), intent(in) :: rhs
      character(len=*), intent(in) :: lhs
      logical lhs_eq_rhs
    end function

    elemental module function string_t_ne_string_t(lhs, rhs) result(lhs_ne_rhs)
      implicit none
      class(string_t), intent(in) :: lhs, rhs
      logical lhs_ne_rhs
    end function

    elemental module function string_t_ne_character(lhs, rhs) result(lhs_ne_rhs)
      implicit none
      class(string_t), intent(in) :: lhs
      character(len=*), intent(in) :: rhs
      logical lhs_ne_rhs
    end function

    elemental module function character_ne_string_t(lhs, rhs) result(lhs_ne_rhs)
      implicit none
      class(string_t), intent(in) :: rhs
      character(len=*), intent(in) :: lhs
      logical lhs_ne_rhs
    end function

    pure module function string_t_cat_string_t(lhs, rhs) result(lhs_cat_rhs)
      implicit none
      class(string_t), intent(in) :: lhs, rhs
      type(string_t) lhs_cat_rhs
    end function

    pure module function string_t_cat_character(lhs, rhs) result(lhs_cat_rhs)
      implicit none
      class(string_t), intent(in) :: lhs
      character(len=*), intent(in) :: rhs
      type(string_t) lhs_cat_rhs
    end function

    pure module function character_cat_string_t(lhs, rhs) result(lhs_cat_rhs)
      implicit none
      character(len=*), intent(in) :: lhs
      class(string_t), intent(in) :: rhs
      type(string_t) lhs_cat_rhs
    end function

    pure module subroutine assign_character_to_string_t(lhs, rhs)
      implicit none
      class(string_t), intent(inout) :: lhs
      character(len=*), intent(in) :: rhs
    end subroutine

    pure module subroutine assign_string_t_to_character(lhs, rhs)
      implicit none
      class(string_t), intent(in) :: rhs
      character(len=:), intent(out), allocatable :: lhs
    end subroutine

    elemental module function bracket(self, opening, closing) result(bracketed_self)
      implicit none
      class(string_t), intent(in) :: self
      character(len=*), intent(in), optional :: opening, closing
      type(string_t) bracketed_self
    end function

  end interface
  
end module julienne_string_m
