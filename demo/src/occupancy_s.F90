! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(occupancy_m) occupancy_s
  use assert_m
  implicit none

contains

  pure function allocations(occupancy) result(components_allocated)

    type(occupancy_t), intent(in) :: occupancy
    logical, allocatable :: components_allocated(:)

    components_allocated = [             &
      allocated(occupancy%occupied_1D_ ) &
     ,allocated(occupancy%occupied_2D_ ) &
     ,allocated(occupancy%occupied_3D_ ) &
     ,allocated(occupancy%occupied_4D_ ) &
     ,allocated(occupancy%occupied_5D_ ) &
     ,allocated(occupancy%occupied_6D_ ) &
     ,allocated(occupancy%occupied_7D_ ) &
     ,allocated(occupancy%occupied_8D_ ) &
     ,allocated(occupancy%occupied_9D_ ) &
     ,allocated(occupancy%occupied_10D_) &
     ,allocated(occupancy%occupied_11D_) &
     ,allocated(occupancy%occupied_12D_) &
     ,allocated(occupancy%occupied_13D_) &
     ,allocated(occupancy%occupied_14D_) &
     ,allocated(occupancy%occupied_15D_) &
    ]

  end function allocations

  module procedure vacate

    select case(size(dims))
    case(1)
      if (allocated(self%occupied_1D_)) deallocate(self%occupied_1D_)
      allocate(self%occupied_1D_(dims(1)), source = .false.)
    case(2)
      if (allocated(self%occupied_2D_)) deallocate(self%occupied_2D_)
      allocate(self%occupied_2D_(dims(1),dims(2)), source = .false.)
    case(3)
      if (allocated(self%occupied_3D_)) deallocate(self%occupied_3D_)
      allocate(self%occupied_3D_(dims(1),dims(2),dims(3)), source = .false.)
    case(4)
      if (allocated(self%occupied_4D_)) deallocate(self%occupied_4D_)
      allocate(self%occupied_4D_(dims(1),dims(2),dims(3),dims(4)), source = .false.)
    case(5)
      if (allocated(self%occupied_5D_)) deallocate(self%occupied_5D_)
      allocate(self%occupied_5D_(dims(1),dims(2),dims(3),dims(4),dims(5)), source = .false.)
    case(6)
      if (allocated(self%occupied_6D_)) deallocate(self%occupied_6D_)
      allocate(self%occupied_6D_(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), source = .false.)
    case(7)
      if (allocated(self%occupied_7D_)) deallocate(self%occupied_7D_)
      allocate(self%occupied_7D_(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), source = .false.)
    case(8)
      if (allocated(self%occupied_8D_)) deallocate(self%occupied_8D_)
      allocate(self%occupied_8D_(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7),dims(8)), source = .false.)
    case(9)
      if (allocated(self%occupied_9D_)) deallocate(self%occupied_9D_)
      allocate(self%occupied_9D_(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7),dims(8),dims(9)), source = .false.)
    case(10)
      if (allocated(self%occupied_10D_)) deallocate(self%occupied_10D_)
      allocate(self%occupied_10D_(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7),dims(8),dims(9),dims(10)), source = .false.)
    case(11)
      if (allocated(self%occupied_11D_)) deallocate(self%occupied_11D_)
      allocate(self%occupied_11D_(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7),dims(8),dims(9),dims(10),dims(11)), source = .false.)
    case(12)
      if (allocated(self%occupied_12D_)) deallocate(self%occupied_12D_)
      allocate(self%occupied_12D_(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7),dims(8),dims(9),dims(10),dims(11),dims(12)), source = .false.)
    case(13)
      if (allocated(self%occupied_13D_)) deallocate(self%occupied_13D_)
      allocate(self%occupied_13D_(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7),dims(8),dims(9),dims(10),dims(11),dims(12),dims(13)), source = .false.)
    case(14)
      if (allocated(self%occupied_14D_)) deallocate(self%occupied_14D_)
      allocate(self%occupied_14D_(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7),dims(8),dims(9),dims(10),dims(11),dims(12),dims(13),dims(14)), source = .false.)
    case(15)
      if (allocated(self%occupied_15D_)) deallocate(self%occupied_15D_)
      allocate(self%occupied_15D_(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7),dims(8),dims(9),dims(10),dims(11),dims(12),dims(13),dims(14),dims(15)), source= .false.)
    case default
      error stop "occupancy_s(vacate): unsupported rank"
    end select

    call_assert(self%allocated_dim()==size(dims))
  
  end procedure vacate

  module procedure occupy
    
    associate_o: &
    associate(o => (loc))
      select case(size(loc))
      case(1)
        self%occupied_1D_(o(1)) = .true.
      case(2)
        self%occupied_2D_(o(1),o(2)) = .true.
      case(3)
        self%occupied_3D_(o(1),o(2),o(3)) = .true.
      case(4)
        self%occupied_4D_(o(1),o(2),o(3),o(4)) = .true.
      case(5)
        self%occupied_5D_(o(1),o(2),o(3),o(4),o(5)) = .true.
      case(6)
        self%occupied_6D_(o(1),o(2),o(3),o(4),o(5),o(6)) = .true.
      case(7)
        self%occupied_7D_(o(1),o(2),o(3),o(4),o(5),o(6),o(7)) = .true.
      case(8)
        self%occupied_8D_(o(1),o(2),o(3),o(4),o(5),o(6),o(7),o(8)) = .true.
      case(9)
        self%occupied_9D_(o(1),o(2),o(3),o(4),o(5),o(6),o(7),o(8),o(9)) = .true.
      case(10)
        self%occupied_10D_(o(1),o(2),o(3),o(4),o(5),o(6),o(7),o(8),o(9),o(10)) = .true.
      case(11)
        self%occupied_11D_(o(1),o(2),o(3),o(4),o(5),o(6),o(7),o(8),o(9),o(10),o(11)) = .true.
      case(12)
        self%occupied_12D_(o(1),o(2),o(3),o(4),o(5),o(6),o(7),o(8),o(9),o(10),o(11),o(12)) = .true.
      case(13)
        self%occupied_13D_(o(1),o(2),o(3),o(4),o(5),o(6),o(7),o(8),o(9),o(10),o(11),o(12),o(13)) = .true.
      case(14)
        self%occupied_14D_(o(1),o(2),o(3),o(4),o(5),o(6),o(7),o(8),o(9),o(10),o(11),o(12),o(13),o(14)) = .true.
      case(15)
        self%occupied_15D_(o(1),o(2),o(3),o(4),o(5),o(6),o(7),o(8),o(9),o(10),o(11),o(12),o(13),o(14),o(15)) = .true.
      case default
        error stop "occupancy_s(occupy): unsupported rank"
      end select
    end associate associate_o

  end procedure occupy

  module procedure occupied

    nickname_loc: &
    associate(b => (loc))
      select case(size(loc))
      case(1)
        bin_occupied = self%occupied_1D_(b(1))
      case(2)
        bin_occupied = self%occupied_2D_(b(1),b(2))
      case(3)
        bin_occupied = self%occupied_3D_(b(1),b(2),b(3))
      case(4)
        bin_occupied = self%occupied_4D_(b(1),b(2),b(3),b(4))
      case(5)
        bin_occupied = self%occupied_5D_(b(1),b(2),b(3),b(4),b(5))
      case(6)
        bin_occupied = self%occupied_6D_(b(1),b(2),b(3),b(4),b(5),b(6))
      case(7)
        bin_occupied = self%occupied_7D_(b(1),b(2),b(3),b(4),b(5),b(6),b(7))
      case(8)
        bin_occupied = self%occupied_8D_(b(1),b(2),b(3),b(4),b(5),b(6),b(7),b(8))
      case(9)
        bin_occupied = self%occupied_9D_(b(1),b(2),b(3),b(4),b(5),b(6),b(7),b(8),b(9))
      case(10)
        bin_occupied = self%occupied_10D_(b(1),b(2),b(3),b(4),b(5),b(6),b(7),b(8),b(9),b(10))
      case(11)
        bin_occupied = self%occupied_11D_(b(1),b(2),b(3),b(4),b(5),b(6),b(7),b(8),b(9),b(10),b(11))
      case(12)
        bin_occupied = self%occupied_12D_(b(1),b(2),b(3),b(4),b(5),b(6),b(7),b(8),b(9),b(10),b(11),b(12))
      case(13)
        bin_occupied = self%occupied_13D_(b(1),b(2),b(3),b(4),b(5),b(6),b(7),b(8),b(9),b(10),b(11),b(12),b(13))
      case(14)
        bin_occupied = self%occupied_14D_(b(1),b(2),b(3),b(4),b(5),b(6),b(7),b(8),b(9),b(10),b(11),b(12),b(13),b(14))
      case(15)
        bin_occupied = self%occupied_15D_(b(1),b(2),b(3),b(4),b(5),b(6),b(7),b(8),b(9),b(10),b(11),b(12),b(13),b(14),b(15))
      case default
        error stop "occupancy_s(occupied): unsupported rank"
      end select
    end associate nickname_loc

  end procedure occupied

  module procedure num_occupied

      call_assert(count(allocations(self))==1)

      select case(self%allocated_dim())
      case(1)
        bins_occupied = count(self%occupied_1D_, kind=int64)
      case(2)
        bins_occupied = count(self%occupied_2D_, kind=int64)
      case(3)
        bins_occupied = count(self%occupied_3D_, kind=int64)
      case(4)
        bins_occupied = count(self%occupied_4D_, kind=int64)
      case(5)
        bins_occupied = count(self%occupied_5D_, kind=int64)
      case(6)
        bins_occupied = count(self%occupied_6D_, kind=int64)
      case(7)
        bins_occupied = count(self%occupied_7D_, kind=int64)
      case(8)
        bins_occupied = count(self%occupied_8D_, kind=int64)
      case(9)
        bins_occupied = count(self%occupied_9D_, kind=int64)
      case(10)
        bins_occupied = count(self%occupied_10D_, kind=int64)
      case(11)
        bins_occupied = count(self%occupied_11D_, kind=int64)
      case(12)
        bins_occupied = count(self%occupied_12D_, kind=int64)
      case(13)
        bins_occupied = count(self%occupied_13D_, kind=int64)
      case(14)
        bins_occupied = count(self%occupied_14D_, kind=int64)
      case(15)
        bins_occupied = count(self%occupied_15D_, kind=int64)
      case default
        error stop "occupancy_s(num_occupied): unsupported rank"
      end select

  end procedure num_occupied

  module procedure num_bins

      select case(self%allocated_dim())
      case(1)
        bins_total = size(self%occupied_1D_)
      case(2)
        bins_total = size(self%occupied_2D_)
      case(3)
        bins_total = size(self%occupied_3D_)
      case(4)
        bins_total = size(self%occupied_4D_)
      case(5)
        bins_total = size(self%occupied_5D_)
      case(6)
        bins_total = size(self%occupied_6D_)
      case(7)
        bins_total = size(self%occupied_7D_)
      case(8)
        bins_total = size(self%occupied_8D_)
      case(9)
        bins_total = size(self%occupied_9D_)
      case(10)
        bins_total = size(self%occupied_10D_)
      case(11)
        bins_total = size(self%occupied_11D_)
      case(12)
        bins_total = size(self%occupied_12D_)
      case(13)
        bins_total = size(self%occupied_13D_)
      case(14)
        bins_total = size(self%occupied_14D_)
      case(15)
        bins_total = size(self%occupied_15D_)
      case default
        error stop "occupancy_s(num_bins): unsupported rank"
      end select

  end procedure num_bins

  module procedure allocated_dim

    associate(my_allocations => allocations(self))
      call_assert(count(my_allocations)==1)
      my_dim = findloc(my_allocations, .true., dim=1)
    end associate

  end procedure

end submodule occupancy_s
