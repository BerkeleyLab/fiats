! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(occupancy_m) occupancy_s
  use julienne_m, only : call_julienne_assert_, operator(.equalsExpected.)
  implicit none

contains

  module procedure construct_occupancy
    occupancy%minima_ = minima
    occupancy%maxima_ = maxima
    occupancy%bins_per_dimension_ = bins_per_dimension
    occupancy%phase_space_bin_ = [phase_space_bin_t::]
  end procedure

  module procedure construct_phase_space_bin

    real, parameter :: half = 0.5

    call_julienne_assert(.all. (maxima .greaterThan. minima))
    call_julienne_assert(.all. (bins_per_dimension .isAtLeast. 1))

    associate(bin_widths => (maxima - minima)/real(bins_per_dimension))
      associate(tensor_values => min(tensor%values(), maxima - half*bin_widths))
        phase_space_bin%loc = (tensor_values - minima)/bin_widths + 1
      end associate
    end associate

  end procedure

  module procedure occupied

    integer b

    do b = 1, size(self%phase_space_bin_,1)
      if (all(self%phase_space_bin_(b)%loc == bin%loc)) then
        bin_occupied = .true.
        return
      end if
    end do

    bin_occupied = .false.

  end procedure

  module procedure add
    self%phase_space_bin_ = [self%phase_space_bin_, bin] ! ToDo: double list instead of growing by one whenever needed
  end procedure

  module procedure num_occupied
    occupied = size(self%phase_space_bin_)
  end procedure

  module procedure num_bins

    call_julienne_assert(size(self%phase_space_bin_) .isAtLeast. 1)

    associate(dimensions => size(self%phase_space_bin_(1)%loc))
      bins = self%bins_per_dimension_**dimensions
    end associate

  end procedure

end submodule occupancy_s
