! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module adon_arrays_m 
  use iso_fortran_env, only : int8, int16, real64, output_unit
  implicit none
  
  private
  public :: adon_arrays_t

  type adon_arrays_t
    real(real64), allocatable :: branch_input(:,:)
    real(real64), allocatable :: branch_output(:,:)
    real(real64), allocatable :: branch_dot_trunk(:,:)
    real(real64), allocatable :: trunk_output(:,:,:)
  end type

  interface adon_arrays_t

    module function read_npy(path) result(adon_arrays)
      implicit none
      character(len=*), intent(in) :: path
      type(adon_arrays_t) adon_arrays
    end function

  end interface

end module adon_arrays_m 
