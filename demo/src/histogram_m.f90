! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module histogram_m
  !! Generate and represent histograms
  use julienne_m, only : file_t
  implicit none

  private
  public :: histogram_t, to_file

  type histogram_t
    !! encapsulate the primary data associated with histograms
    private
    character(len=:), allocatable :: variable_name_
    real unmapped_min_, unmapped_max_
    real, allocatable :: bin_frequency_(:), bin_value_(:)
  contains
    procedure variable_name
  end type

  interface histogram_t

    pure module function construct(v, variable_name, num_bins, raw) result(histogram)
      implicit none
      real, intent(in) :: v(:,:,:,:)
      character(len=*), intent(in) :: variable_name
      integer, intent(in) :: num_bins
      logical, intent(in) :: raw
      type(histogram_t) histogram
    end function

  end interface

  interface to_file

    pure module function to_separate_file(histogram) result(file)
      implicit none
      type(histogram_t), intent(in) :: histogram
      type(file_t) file
    end function

    pure module function to_common_file(histograms) result(file)
      implicit none
      type(histogram_t), intent(in) :: histograms(:)
      type(file_t) file
    end function

  end interface

  interface

    pure module function variable_name(self) result(name)
      implicit none
      class(histogram_t), intent(in) :: self
      character(len=:), allocatable :: name
    end function

  end interface

end module histogram_m
