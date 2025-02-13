! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module time_data_m
  use kind_parameters_m, only : default_real
  use julienne_m, only : file_t, string_t
  implicit none

  private
  public :: time_data_t
  public :: icar_output_file_t

  type time_data_t!(k)
    !! encapsulate separately saved ICAR time stamps and steps
    !integer, kind :: k = default_real
    type(string_t), allocatable, private :: date_(:), time_(:)
    real, allocatable, private :: dt_(:)
    !real(k), allocatable :: dt_(:)
  contains
    generic :: to_json => default_real_to_json
    procedure, private :: default_real_to_json
    generic :: dt      => default_real_dt 
    procedure, private :: default_real_dt
  end type

  type, extends(file_t) :: icar_output_file_t
  end type

  interface time_data_t

    pure module function default_real_from_json(file) result(time_data)
      implicit none
      type(file_t), intent(in) :: file
      type(time_data_t) time_data
    end function

    pure module function default_real_from_strings(date, time, dt) result(time_data)
      implicit none
      type(string_t), intent(in) :: date(:), time(:)
      real, intent(in) :: dt(:)
      type(time_data_t) time_data
    end function

    pure module function default_real_from_characters(date, time, dt) result(time_data)
      implicit none
      character(len=*), intent(in) :: date(:), time(:)
      real, intent(in) :: dt(:)
      type(time_data_t) time_data
    end function

    pure module function from_icar_output(icar_output_file) result(time_data)
      implicit none
      type(icar_output_file_t), intent(in) :: icar_output_file 
      type(time_data_t) time_data
    end function

  end interface

  interface icar_output_file_t

    pure module function from_file_object(file) result(icar_output_file)
      implicit none
      type(file_t), intent(in) :: file
      type(icar_output_file_t) icar_output_file
    end function

  end interface

  interface

    pure module function default_real_to_json(self) result(file)
      implicit none
      class(time_data_t), intent(in) :: self
      type(file_t) file
    end function

    pure module function default_real_dt(self) result(dt_values)
      implicit none
      class(time_data_t), intent(in) :: self
      real, allocatable :: dt_values(:)
    end function

  end interface

end module time_data_m
