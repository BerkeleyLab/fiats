! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(NetCDF_variable_m) NetCDF_variable_s
  use kind_parameters_m, only : default_real
  use assert_m, only : assert, intrinsic_array_t
  implicit none

  interface components_allocated
    module procedure default_real_components_allocated
    module procedure double_precision_components_allocated
  end interface

  interface lower_bounds
    module procedure default_real_lower_bounds
    module procedure double_precision_lower_bounds
  end interface

  interface upper_bounds
    module procedure default_real_upper_bounds
    module procedure double_precision_upper_bounds
  end interface

contains

  module procedure default_real_copy

    if (present(rename)) then
      NetCDF_variable%name_ = rename
    else
      NetCDF_variable%name_ = source%name_
    end if

    select case(source%rank())
    case (1)
      NetCDF_variable%values_1D_ = source%values_1D_
    case (2)
      NetCDF_variable%values_2D_ = source%values_2D_
    case (3)
      NetCDF_variable%values_3D_ = source%values_3D_
    case (4)
      NetCDF_variable%values_4D_ = source%values_4D_
    case default
      error stop 'NetCDF_variable_s(default_real_copy): unsupported rank'
    end select 

  end procedure

  module procedure default_real_copy_character_name
    NetCDF_variable = default_real_copy(source, string_t(rename))
  end procedure
  
  module procedure double_precision_copy

    if (present(rename)) then
      NetCDF_variable%name_ = rename
    else
      NetCDF_variable%name_ = source%name_
    end if

    select case(source%rank())
    case (1)
      NetCDF_variable%values_1D_ = source%values_1D_
    case (2)
      NetCDF_variable%values_2D_ = source%values_2D_
    case (3)
      NetCDF_variable%values_3D_ = source%values_3D_
    case (4)
      NetCDF_variable%values_4D_ = source%values_4D_
    case default
      error stop 'NetCDF_variable_s(double_precision_copy): unsupported rank'
    end select 

  end procedure

  module procedure double_precision_copy_character_name
    NetCDF_variable = double_precision_copy(source, string_t(rename))
  end procedure
  
  module procedure default_real_input
    self%name_ = variable_name
    select case (rank)
    case (1)
      call file%input(variable_name%string(), self%values_1D_)
    case (2)
      call file%input(variable_name%string(), self%values_2D_)
    case (3)
      call file%input(variable_name%string(), self%values_3D_)
    case (4)
      call file%input(variable_name%string(), self%values_4D_)
    case default
      error stop 'NetCDF_variable_s(default_real_input): unsupported rank'
    end select 
  end procedure
  
  module procedure default_real_input_character_name
    call self%default_real_input(string_t(variable_name), file, rank)
  end procedure

  module procedure double_precision_input
    self%name_ = variable_name
    select case (rank)
    case (1)
      call file%input(variable_name%string(), self%values_1D_)
    case (2)
      call file%input(variable_name%string(), self%values_2D_)
    case (3)
      call file%input(variable_name%string(), self%values_3D_)
    case (4)
      call file%input(variable_name%string(), self%values_4D_)
    case default
      error stop 'NetCDF_variable_s(double_precision_input): unsupported rank'
    end select
  end procedure

  module procedure double_precision_input_character_name
    call self%double_precision_input(string_t(variable_name), file, rank)
  end procedure

  pure function default_real_components_allocated(NetCDF_variable) result(allocation_vector)
    type(NetCDF_variable_t), intent(in) :: NetCDF_variable
    logical, allocatable :: allocation_vector(:)
    allocation_vector = [ allocated(NetCDF_variable%values_1D_) &
                         ,allocated(NetCDF_variable%values_2D_) &
                         ,allocated(NetCDF_variable%values_3D_) &
                         ,allocated(NetCDF_variable%values_4D_) ]
  end function

  pure function double_precision_components_allocated(NetCDF_variable) result(allocation_vector)
    type(NetCDF_variable_t(double_precision)), intent(in) :: NetCDF_variable
    logical, allocatable :: allocation_vector(:)
    allocation_vector = [ allocated(NetCDF_variable%values_1D_) &
                         ,allocated(NetCDF_variable%values_2D_) &
                         ,allocated(NetCDF_variable%values_3D_) &
                         ,allocated(NetCDF_variable%values_4D_) ]
  end function

  module procedure default_real_rank
    associate(allocation_vector => components_allocated(self))
      call assert(count(allocation_vector) == 1, "NetCDF_variable_s(default_real_rank): allocation count")
      my_rank = findloc(allocation_vector, .true., dim=1)
    end associate
  end procedure

  module procedure double_precision_rank
    associate(allocation_vector => components_allocated(self))
      call assert(count(allocation_vector) == 1, "NetCDF_variable_s(double_precision_rank): allocation count")
      my_rank = findloc(allocation_vector, .true., dim=1)
    end associate
  end procedure

  pure function default_real_lower_bounds(NetCDF_variable) result(lbounds)
    type(NetCDF_variable_t), intent(in) :: NetCDF_variable
    integer, allocatable :: lbounds(:)
    select case(NetCDF_variable%rank())
    case(1)
      lbounds = lbound(NetCDF_variable%values_1D_)
    case(2)
      lbounds = lbound(NetCDF_variable%values_2D_)
    case(3)
      lbounds = lbound(NetCDF_variable%values_3D_)
    case(4)
      lbounds = lbound(NetCDF_variable%values_4D_)
    case default
      error stop "NetCDF_variable_s(default_real_lower_bounds): unsupported rank"
    end select
  end function

  pure function double_precision_lower_bounds(NetCDF_variable) result(lbounds)
    type(NetCDF_variable_t(double_precision)), intent(in) :: NetCDF_variable
    integer, allocatable :: lbounds(:)
    select case(NetCDF_variable%rank())
    case(1)
      lbounds = lbound(NetCDF_variable%values_1D_)
    case(2)
      lbounds = lbound(NetCDF_variable%values_2D_)
    case(3)
      lbounds = lbound(NetCDF_variable%values_3D_)
    case(4)
      lbounds = lbound(NetCDF_variable%values_4D_)
    case default
      error stop "NetCDF_variable_s(double_precision_lower_bounds): unsupported rank"
    end select
  end function

  pure function default_real_upper_bounds(NetCDF_variable) result(ubounds)
    type(NetCDF_variable_t), intent(in) :: NetCDF_variable
    integer, allocatable :: ubounds(:)
    select case(NetCDF_variable%rank())
    case(1)
      ubounds = ubound(NetCDF_variable%values_1D_)
    case(2)
      ubounds = ubound(NetCDF_variable%values_2D_)
    case(3)
      ubounds = ubound(NetCDF_variable%values_3D_)
    case(4)
      ubounds = ubound(NetCDF_variable%values_4D_)
    case default
      error stop "NetCDF_variable_s(default_real_upper_bounds): unsupported rank"
    end select
  end function

  pure function double_precision_upper_bounds(NetCDF_variable) result(ubounds)
    type(NetCDF_variable_t(double_precision)), intent(in) :: NetCDF_variable
    integer, allocatable :: ubounds(:)
    select case(NetCDF_variable%rank())
    case(1)
      ubounds = ubound(NetCDF_variable%values_1D_)
    case(2)
      ubounds = ubound(NetCDF_variable%values_2D_)
    case(3)
      ubounds = ubound(NetCDF_variable%values_3D_)
    case(4)
      ubounds = ubound(NetCDF_variable%values_4D_)
    case default
      error stop "NetCDF_variable_s(double_precision_upper_bounds): unsupported rank"
    end select
  end function

  module procedure default_real_conformable_with
    conformable = all([ self%rank()        == NetCDF_variable%rank()         &
                       ,lower_bounds(self) == lower_bounds(NetCDF_variable)  &
                       ,upper_bounds(self) == upper_bounds(NetCDF_variable) ]) 
  end procedure

  module procedure double_precision_conformable_with
    conformable = all([ self%rank()        == NetCDF_variable%rank()         &
                       ,lower_bounds(self) == lower_bounds(NetCDF_variable)  &
                       ,upper_bounds(self) == upper_bounds(NetCDF_variable) ]) 
  end procedure

  module procedure default_real_subtract
    call assert(lhs%conformable_with(rhs), "NetCDF_variable_s(default_real_subtract): lhs%conformable_with(rhs)")
    select case(lhs%rank())
    case(1)
      difference%values_1D_ = lhs%values_1D_ - rhs%values_1D_
    case(2)
      difference%values_2D_ = lhs%values_2D_ - rhs%values_2D_
    case(3)
      difference%values_3D_ = lhs%values_3D_ - rhs%values_3D_
    case(4)
      difference%values_4D_ = lhs%values_4D_ - rhs%values_4D_
    case default
      error stop "NetCDF_variable_s(default_real_subtract): unsupported rank)"
    end select
  end procedure

  module procedure double_precision_subtract
    call assert(lhs%conformable_with(rhs), "NetCDF_variable_s(double_precision_subtract): lhs%conformable_with(rhs)")
    select case(lhs%rank())
    case(1)
      difference%values_1D_ = lhs%values_1D_ - rhs%values_1D_
    case(2)
      difference%values_2D_ = lhs%values_2D_ - rhs%values_2D_
    case(3)
      difference%values_3D_ = lhs%values_3D_ - rhs%values_3D_
    case(4)
      difference%values_4D_ = lhs%values_4D_ - rhs%values_4D_
    case default
      error stop "NetCDF_variable_s(double_precision_subtract): unsupported rank)"
    end select
  end procedure

  module procedure default_real_divide

    integer t

    call assert(rhs%rank()==1, "NetCDF_variable_s(default_real_divide): rhs%rank()==1")

    associate(t_end => size(rhs%values_1D_))

      select case(lhs%rank())
      case(4)

        call assert(size(rhs%values_1D_) == size(lhs%values_4D_,4), "NetCDF_variable_s(default_real_divide): conformable numerator/denominator")
        allocate(ratio%values_4D_, mold = lhs%values_4D_)

        do concurrent(t = 1:t_end)
          ratio%values_4D_(:,:,:,t) = lhs%values_4D_(:,:,:,t) / rhs%values_1D_(t)
        end do

      case default
        error stop "NetCDF_variable_s(default_real_divide): unsupported lhs rank)"
      end select

    end associate

  end procedure

  module procedure double_precision_divide

    integer t

    call assert(rhs%rank()==1, "NetCDF_variable_s(double_precision_divide): rhs%rank()==1")

    associate(t_end => size(rhs%values_1D_))

      select case(lhs%rank())
      case(4)

        call assert(size(rhs%values_1D_) == size(lhs%values_4D_,4), "NetCDF_variable_s(double_precision_divide): conformable numerator/denominator")
        allocate(ratio%values_4D_, mold = lhs%values_4D_)

        do concurrent(t = 1:t_end)
          ratio%values_4D_(:,:,:,t) = lhs%values_4D_(:,:,:,t) / rhs%values_1D_(t)
        end do

      case default
        error stop "NetCDF_variable_s(double_precision_divide): unsupported lhs rank)"
      end select

    end associate

  end procedure

  module procedure default_real_assign
    select case(rhs%rank())
    case(1)
      lhs%values_1D_ = rhs%values_1D_
    case(2)
      lhs%values_2D_ = rhs%values_2D_
    case(3)
      lhs%values_3D_ = rhs%values_3D_
    case(4)
      lhs%values_4D_ = rhs%values_4D_
    case default
      error stop "NetCDF_variable_s(default_real_assign): unsupported rank)"
    end select
    call assert(lhs%rank()==rhs%rank(), "NetCDF_variable_s(default_real_assign): ranks match)")
  end procedure

  module procedure double_precision_assign
    select case(rhs%rank())
    case(1)
      lhs%values_1D_ = rhs%values_1D_
    case(2)
      lhs%values_2D_ = rhs%values_2D_
    case(3)
      lhs%values_3D_ = rhs%values_3D_
    case(4)
      lhs%values_4D_ = rhs%values_4D_
    case default
      error stop "NetCDF_variable_s(double_precision_assign): unsupported rank)"
    end select
    call assert(lhs%rank()==rhs%rank(), "NetCDF_variable_s(double_precision_assign): ranks match)")
  end procedure

end submodule NetCDF_variable_s