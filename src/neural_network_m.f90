module stuff_m
  implicit none
  
  type string_t
  contains
    generic :: assignment(=) => assign_string_t_to_character
    procedure assign_string_t_to_character
  end type

  interface
    module subroutine assign_string_t_to_character(lhs, rhs)
      implicit none
      class(string_t), intent(out) :: lhs
      character(len=*), intent(in) :: rhs
    end subroutine
  end interface

  integer, parameter :: default_real = kind(1.), double_precision = kind(1D0)

  type double_precision_file_t
  end type

  type metadata_t
    type(string_t) modelName_
  end type

  type neural_network_t(k)
    integer, kind :: k = default_real 
    type(metadata_t) metadata_
    real(k), allocatable :: weight_
  end type

  interface neural_network_t
    module function double_precision_from_json(file) result(neural_network)
      implicit none
      type(double_precision_file_t) file
      type(neural_network_t(double_precision)) neural_network
    end function
  end interface

  type unmapped_network_t(k)
    integer, kind :: k = default_real
    type(neural_network_t(k)) neural_network_
  end type

  interface unmapped_network_t
    module function double_precision_unmapped_from_json(file) result(unmapped_network)
      implicit none
      type(double_precision_file_t) file
      type(unmapped_network_t(double_precision)) unmapped_network
    end function
  end interface

contains

  module procedure double_precision_unmapped_from_json
    unmapped_network%neural_network_ = double_precision_from_json(file)
  end procedure

end module
