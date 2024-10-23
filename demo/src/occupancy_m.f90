module occupancy_m
  use iso_fortran_env, only : int64
  implicit none

  private
  public :: occupancy_t

 type occupancy_t
   private
   logical, allocatable :: occupied_1D_(:)
   logical, allocatable :: occupied_2D_(:,:)
   logical, allocatable :: occupied_3D_(:,:,:)
   logical, allocatable :: occupied_4D_(:,:,:,:)
   logical, allocatable :: occupied_5D_(:,:,:,:,:)
   logical, allocatable :: occupied_6D_(:,:,:,:,:,:)
   logical, allocatable :: occupied_7D_(:,:,:,:,:,:,:)
   logical, allocatable :: occupied_8D_(:,:,:,:,:,:,:,:)
   logical, allocatable :: occupied_9D_(:,:,:,:,:,:,:,:,:)
   logical, allocatable :: occupied_10D_(:,:,:,:,:,:,:,:,:,:)
   logical, allocatable :: occupied_11D_(:,:,:,:,:,:,:,:,:,:,:)
   logical, allocatable :: occupied_12D_(:,:,:,:,:,:,:,:,:,:,:,:)
   logical, allocatable :: occupied_13D_(:,:,:,:,:,:,:,:,:,:,:,:,:)
   logical, allocatable :: occupied_14D_(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
   logical, allocatable :: occupied_15D_(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
 contains
   procedure, non_overridable :: vacate
   procedure, non_overridable :: occupy
   procedure, non_overridable :: occupied
   procedure, non_overridable :: num_occupied
   procedure, non_overridable :: num_bins
   procedure, non_overridable :: allocated_dim
 end type

 interface

   pure module subroutine vacate(self, dims)
     implicit none
     class(occupancy_t), intent(inout) :: self
     integer, intent(in) :: dims(:)
   end subroutine

   pure module subroutine occupy(self, loc)
     implicit none
     class(occupancy_t), intent(inout) :: self
     integer, intent(in) :: loc(:)
   end subroutine

   pure module function occupied(self, loc) result(bin_occupied)
     implicit none
     class(occupancy_t), intent(in) :: self
     integer, intent(in) :: loc(:)
     logical bin_occupied
   end function

   pure module function num_occupied(self) result(bins_occupied)
     implicit none
     class(occupancy_t), intent(in) :: self
     integer(int64) bins_occupied
   end function

   pure module function num_bins(self) result(bins_total)
     implicit none
     class(occupancy_t), intent(in) :: self
     integer(int64) bins_total
   end function

   pure module function allocated_dim(self) result(my_dim)
     implicit none
     class(occupancy_t), intent(in) :: self
     integer my_dim
   end function

 end interface
  
end module occupancy_m
