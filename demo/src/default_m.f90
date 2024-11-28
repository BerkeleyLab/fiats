module default_m
  !! Define functions to choose between default values and alternative values
  implicit none     

  private
  public :: default_or_internal_read
  public :: default_or_present_value

  interface default_or_internal_read

    pure module function default_integer_or_internal_read(default_value, string) result(set_value)
      !! result is read from string if len(string)/=0, default_value otherwise
      implicit none
      integer, intent(in) :: default_value
      character(len=*), intent(in) :: string
      integer set_value
    end function 

    pure module function default_real_or_internal_read(default_value, string) result(set_value)
      !! result is read from string if len(string)/=0, default_value otherwise
      implicit none
      real, intent(in) :: default_value
      character(len=*), intent(in) :: string
      real set_value
    end function

  end interface

  interface default_or_present_value

    pure module function default_integer_or_present_value(default_value, optional_value) result(set_value)
      !! result is default_value if optional_value not present, optional_value otherwise
      implicit none
      integer, intent(in) :: default_value
      integer, intent(in), optional :: optional_value
      integer set_value
    end function 

    pure module function default_real_or_present_value(default_value, optional_value) result(set_value)
      !! result is default_value if optional_value not present, optional_value otherwise
      implicit none
      real, intent(in) :: default_value
      real, intent(in), optional :: optional_value
      real set_value
    end function

  end interface

end module
