! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_string_m
  use iso_c_binding, only : c_bool, c_size_t
  implicit none
  
  private
  public :: string_t
  public :: operator(.csv.)  ! comma-separated values unary operator
  public :: operator(.separatedBy.), operator(.sv.)   ! separated-values binary operator

  type string_t
    private
    character(len=:), allocatable :: string_
  contains
    procedure :: as_character
    generic :: string => as_character
    procedure :: get_json_key
    procedure :: bracket
    generic :: operator(//)   => string_t_cat_string_t, string_t_cat_character, character_cat_string_t
    generic :: operator(==)   => string_t_eq_string_t, string_t_eq_character
    generic :: assignment(= ) => assign_string_t_to_character, assign_character_to_string_t
    generic :: get_json_value => get_string_with_string_key &
                                ,get_character_with_string_key &
                                ,get_string_t_array_with_character_key, get_string_t_array_with_string_t_key &
                                ,get_real &
                                ,get_logical &
                                ,get_real_array &
                                ,get_integer_array &
                                ,get_integer &
                                ,get_double_precision
    procedure, private :: get_string_with_string_key
    procedure, private :: get_character_with_string_key
    procedure, private :: get_string_t_array_with_character_key, get_string_t_array_with_string_t_key
    procedure, private :: get_real 
    procedure, private :: get_logical 
    procedure, private :: get_integer 
    procedure, private :: get_real_array 
    procedure, private :: get_integer_array 
    procedure, private :: get_double_precision 
    procedure, private :: string_t_eq_string_t, string_t_eq_character
    procedure, private :: assign_character_to_string_t
    procedure, private :: string_t_cat_string_t, string_t_cat_character
    procedure, private, pass(rhs) :: character_cat_string_t
    procedure, private, pass(rhs) :: assign_string_t_to_character
  end type


  interface string_t

    elemental module function from_characters(string) result(new_string)
      implicit none
      character(len=*), intent(in) :: string
      type(string_t) new_string
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

  end interface

  interface

    pure module function strings_with_string_t_separator(strings, separator) result(sv)
      implicit none
      type(string_t), intent(in) :: strings(:), separator
      type(string_t) sv 
    end function

  end interface

  interface operator(.separatedBy.)
    module procedure strings_with_character_separator, strings_with_string_t_separator
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

    pure module function get_double_precision(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      double precision, intent(in) :: mold
      double precision value_
    end function

    pure module function get_character_with_string_key(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      character(len=*), intent(in) :: mold
      character(len=:), allocatable :: value_
    end function

    pure module function get_string_with_string_key(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key, mold
      type(string_t) :: value_
    end function

    pure module function get_string_t_array_with_string_t_key(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self
      type(string_t), intent(in) :: key, mold(:)
      type(string_t), allocatable :: value_(:)
    end function

    pure module function get_string_t_array_with_character_key(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self
      character(len=*), intent(in) :: key
      type(string_t), intent(in) :: mold(:)
      type(string_t), allocatable :: value_(:)
    end function

    pure module function get_integer(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      integer, intent(in) ::  mold
      integer value_
    end function

    pure module function get_logical(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      logical, intent(in) :: mold
      logical value_
    end function

    pure module function get_integer_array(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      integer, intent(in) :: mold(:)
      integer, allocatable :: value_(:)
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
