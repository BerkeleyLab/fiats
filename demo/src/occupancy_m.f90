! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module occupancy_m
  use iso_fortran_env, only : int64
  use tensor_m, only : tensor_t
  implicit none

  private
  public :: occupancy_t
  public :: phase_space_bin_t

  integer, parameter :: default_kind = kind(1)

  type phase_space_bin_t
    integer, allocatable :: loc(:)
  end type
 
  interface phase_space_bin_t

    pure module function construct_phase_space_bin(tensor, minima, maxima, bins_per_dimension) result(phase_space_bin)
      implicit none
      type(tensor_t), intent(in) :: tensor
      real, intent(in) :: minima(:), maxima(:)
      integer, intent(in) :: bins_per_dimension
      type(phase_space_bin_t) phase_space_bin
    end function

  end interface

  type occupancy_t
    !! Encapsulate the occupancy status of phase-space bins
    private
    type(phase_space_bin_t), allocatable :: phase_space_bin_(:)
    real, allocatable :: minima_(:), maxima_(:)
    integer bins_per_dimension_, num_occupied_
  contains
    procedure, non_overridable :: occupied
    procedure, non_overridable :: add
    procedure, non_overridable :: num_occupied
    procedure, non_overridable :: num_bins
  end type

  interface occupancy_t

    module pure function construct_occupancy(minima, maxima, bins_per_dimension) result(occupancy)
      implicit none
      real, intent(in) :: minima(:), maxima(:)
      integer, intent(in) ::  bins_per_dimension
      type(occupancy_t) occupancy
    end function

  end interface

  interface

    module subroutine add(self, bin)
      !! Add bin to list of occupied bins
      implicit none
      class(occupancy_t), intent(inout) :: self
      type(phase_space_bin_t), intent(in) :: bin
    end subroutine

    pure module function occupied(self, bin) result(bin_occupied)
      !! Result is true if the provided bin location has been occupied
      implicit none
      class(occupancy_t), intent(in) :: self
      type(phase_space_bin_t), intent(in) :: bin
      logical bin_occupied
    end function

    pure module function num_occupied(self) result(occupied)
      !! Result is the number of occupied bins
      implicit none
      class(occupancy_t), intent(in) :: self
      integer occupied
    end function

    pure module function num_bins(self) result(bins)
      !! Result is the total number of bins
      implicit none
      class(occupancy_t), intent(in) :: self
      integer(int64) bins
    end function

  end interface
  
end module occupancy_m
