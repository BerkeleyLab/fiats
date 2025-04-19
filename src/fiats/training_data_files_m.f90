! Copyright (c) 2023-2025, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module training_data_files_m
  use julienne_m, only : string_t, file_t
  implicit none

  private
  public :: training_data_files_t

  type training_data_files_t
    private
    character(len=:), allocatable :: path_, inputs_prefix_, outputs_prefix_
    type(string_t), allocatable :: infixes_(:)
  contains
    procedure :: to_json
    procedure :: fully_qualified_inputs_files
    procedure :: fully_qualified_outputs_files
    procedure :: fully_qualified_time_file
    procedure :: path
    generic :: operator(==) => equals
    procedure, private :: equals
  end type

  interface training_data_files_t

    pure module function from_json(file) result(training_data_files)
      implicit none
      type(file_t), intent(in) :: file
      type(training_data_files_t) training_data_files
    end function

    pure module function from_components(path, inputs_prefix, outputs_prefix, infixes) result(training_data_files)
      implicit none
      character(len=*), intent(in)  :: path, inputs_prefix, outputs_prefix
      type(string_t), intent(in) :: infixes(:)
      type(training_data_files_t) training_data_files
    end function

  end interface

  interface

    pure module function path(self) result(training_data_file_path)
      implicit none
      class(training_data_files_t), intent(in) :: self
      character(len=:), allocatable :: training_data_file_path
    end function

    elemental module function equals(lhs, rhs) result(lhs_eq_rhs)
      implicit none
      class(training_data_files_t), intent(in) :: lhs, rhs
      logical lhs_eq_rhs
    end function

    pure module function to_json(self) result(lines)
      implicit none
      class(training_data_files_t), intent(in) :: self
      type(string_t), allocatable :: lines(:)
    end function

    pure module function fully_qualified_inputs_files(self) result(names)
      implicit none
      class(training_data_files_t), intent(in) :: self
      type(string_t), allocatable :: names(:)
    end function

    pure module function fully_qualified_outputs_files(self) result(names)
      implicit none
      class(training_data_files_t), intent(in) :: self
      type(string_t), allocatable :: names(:)
    end function

    pure module function fully_qualified_time_file(self) result(name)
      implicit none
      class(training_data_files_t), intent(in) :: self
      type(string_t) name
    end function

  end interface

end module
