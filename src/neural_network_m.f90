module neural_network_m
  use double_precision_file_m, only : double_precision_file_t
  use kind_parameters_m, only : default_real, double_precision
  use metadata_m, only : metadata_t
  implicit none

  private
  public :: neural_network_t

  type neural_network_t(k)
    integer, kind :: k = default_real 
    type(metadata_t), private :: metadata_
    real(k), allocatable, private :: weights_(:,:,:), biases_(:,:)
  end type

  interface neural_network_t

    impure elemental module function double_precision_from_json(file) result(neural_network)
      implicit none
      type(double_precision_file_t), intent(in) :: file
      type(neural_network_t(double_precision)) neural_network
    end function

  end interface

  type unmapped_network_t(k)
    integer, kind :: k = default_real
    private
    type(neural_network_t(k)) neural_network_
  end type

  interface unmapped_network_t

    impure elemental module function double_precision_unmapped_from_json(file) result(unmapped_network)
      implicit none
      type(double_precision_file_t), intent(in) :: file
      type(unmapped_network_t(double_precision)) unmapped_network
    end function

  end interface

end module neural_network_m
