! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(NetCDF_variable_m) NetCDF_variable_s
  use ieee_arithmetic, only : ieee_is_nan
  use kind_parameters_m, only : default_real
  use julienne_m, only : call_julienne_assert_, operator(.equalsExpected.)
  use default_m, only : default_or_present_value
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

  module procedure default_real_time_derivative

    call_julienne_assert(new%conformable_with(old))
    call_julienne_assert(old%name_ .equalsExpected. new%name_)

    time_derivative%name_ = "d" // old%name_ // "_dt"

    block
      integer t

      select case(old%rank())
      case(1)
        allocate(time_derivative%values_1D_, mold = old%values_1D_)
        do concurrent(t = 1:size(old%values_1D_))
          time_derivative%values_1D_(t) = (new%values_1D_(t) - old%values_1D_(t))/ dt(t)
        end do
      case(2)
        allocate(time_derivative%values_2D_, mold = old%values_2D_)
        do concurrent(t = 1:size(old%values_2D_,2))
          time_derivative%values_2D_(:,t) = (new%values_2D_(:,t) - old%values_2D_(:,t))/ dt(t)
        end do
      case(3)
        allocate(time_derivative%values_3D_, mold = old%values_3D_)
        do concurrent(t = 1:size(old%values_3D_,3))
          time_derivative%values_3D_(:,:,t) = (new%values_3D_(:,:,t) - old%values_3D_(:,:,t))/ dt(t)
        end do
      case(4)
        allocate(time_derivative%values_4D_, mold = old%values_4D_)
        do concurrent(t = 1:size(old%values_4D_,4))
          time_derivative%values_4D_(:,:,:,t) = (new%values_4D_(:,:,:,t) - old%values_4D_(:,:,:,t))/ dt(t)
        end do
      case default
        error stop "NetCDF_variable_s(default_real_time_derivative): unsupported rank)"
      end select
    end block

   end procedure

  module procedure default_real_histogram
    select case(self%rank())
    case (4)
      histogram = histogram_t(self%values_4D_ , self%name_, num_bins, raw)
    case default
      error stop 'NetCDF_variable_s(default_real_histogram): unsupported rank'
    end select 
  end procedure 

  module procedure double_precision_histogram
      error stop 'NetCDF_variable_s(double_precision_histogram): unsupported rank'
  end procedure 

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
      call_julienne_assert(count(allocation_vector) .equalsExpected. 1)
      my_rank = findloc(allocation_vector, .true., dim=1)
    end associate
  end procedure

  module procedure double_precision_rank
    associate(allocation_vector => components_allocated(self))
      call_julienne_assert(count(allocation_vector) .equalsExpected. 1)
      my_rank = findloc(allocation_vector, .true., dim=1)
    end associate
  end procedure

  module procedure default_real_end_step
    select case(self%rank())
    case(1)
      end_step = ubound(self%values_1D_,1)
    case(2)
      end_step = ubound(self%values_2D_,2)
    case(3)
      end_step = ubound(self%values_3D_,3)
    case(4)
      end_step = ubound(self%values_4D_,4)
    case default
      error stop "NetCDF_variable_s(default_real_end_step): unsupported rank"
    end select
  end procedure

  module procedure double_precision_end_step
    select case(self%rank())
    case(1)
      end_step = ubound(self%values_1D_,1)
    case(2)
      end_step = ubound(self%values_2D_,2)
    case(3)
      end_step = ubound(self%values_3D_,3)
    case(4)
      end_step = ubound(self%values_4D_,4)
    case default
      error stop "NetCDF_variable_s(double_precision_end_step): unsupported rank"
    end select
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

    call_julienne_assert(lhs%conformable_with(rhs))
    call_julienne_assert(lhs%name_ .equalsExpected. rhs%name_)

    difference%name_ = lhs%name_

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

    call_julienne_assert(lhs%conformable_with(rhs))
    call_julienne_assert(lhs%name_ .equalsExpected. rhs%name_)

    difference%name_ = lhs%name_

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

  module procedure default_real_any_nan

    select case(self%rank())
    case(1)
      any_nan = any(ieee_is_nan(self%values_1D_))
    case(2)
      any_nan = any(ieee_is_nan(self%values_2D_))
    case(3)
      any_nan = any(ieee_is_nan(self%values_3D_))
    case(4)
      any_nan = any(ieee_is_nan(self%values_4D_))
    case default
      error stop "NetCDF_variable_s(default_real_any_nan): unsupported rank)"
    end select
  end procedure

  module procedure double_precision_any_nan

    select case(self%rank())
    case(1)
      any_nan = any(ieee_is_nan(self%values_1D_))
    case(2)
      any_nan = any(ieee_is_nan(self%values_2D_))
    case(3)
      any_nan = any(ieee_is_nan(self%values_3D_))
    case(4)
      any_nan = any(ieee_is_nan(self%values_4D_))
    case default
      error stop "NetCDF_variable_s(double_precision_any_nan): unsupported rank)"
    end select
  end procedure

  module procedure tensors

    integer t_start, t_end, t_stride

    select case(NetCDF_variables(1)%rank())
    case(4)

      t_start  = default_or_present_value(1, step_start )
      t_stride = default_or_present_value(1, step_stride)
      t_end    = default_or_present_value(size(NetCDF_variables(1)%values_4D_,4), step_end)

      associate( longitudes => size(NetCDF_variables(1)%values_4D_,1) &
                ,latitudes  => size(NetCDF_variables(1)%values_4D_,2) &
                ,levels     => size(NetCDF_variables(1)%values_4D_,3) &
      )
        block
          integer v, lon, lat, lev, time

          tensors = [( [( [( [( tensor_t( [( NetCDF_variables(v)%values_4D_(lon,lat,lev,time),  v=1,size(NetCDF_variables) )] ), &
                                lon = 1, longitudes)], lat = 1, latitudes)], lev = 1, levels)], time = t_start, t_end, t_stride)]
        end block
      end associate    

    case default
      error stop "NetCDF_variable_s(tensors): unsupported rank)"
    end select

  end procedure

  module procedure default_real_end_time
    select case(self%rank())
    case (1)
      end_time = size(self%values_1D_,1)
    case (2)
      end_time = size(self%values_2D_,2)
    case (3)
      end_time = size(self%values_3D_,3)
    case (4)
      end_time = size(self%values_4D_,4)
    case default
      error stop 'NetCDF_variable_s(default_real_end_time): unsupported rank'
    end select 
  end procedure

  module procedure double_precision_end_time
    select case(self%rank())
    case (1)
      end_time = size(self%values_1D_,1)
    case (2)
      end_time = size(self%values_2D_,2)
    case (3)
      end_time = size(self%values_3D_,3)
    case (4)
      end_time = size(self%values_4D_,4)
    case default
      error stop 'NetCDF_variable_s(double_precision_end_time): unsupported rank'
    end select 
  end procedure

  module procedure default_real_minimum
    select case(self%rank())
    case (1)
      minimum = minval(self%values_1D_)
    case (2)
      minimum = minval(self%values_2D_)
    case (3)
      minimum = minval(self%values_3D_)
    case (4)
      minimum = minval(self%values_4D_)
    case default
      error stop 'NetCDF_variable_s(default_real_minimum): unsupported rank'
    end select 
  end procedure

  module procedure double_precision_minimum
    select case(self%rank())
    case (1)
      minimum = minval(self%values_1D_)
    case (2)
      minimum = minval(self%values_2D_)
    case (3)
      minimum = minval(self%values_3D_)
    case (4)
      minimum = minval(self%values_4D_)
    case default
      error stop 'NetCDF_variable_s(double_precision_minimum): unsupported rank'
    end select 
  end procedure

  module procedure default_real_maximum
    select case(self%rank())
    case (1)
      maximum = maxval(self%values_1D_)
    case (2)
      maximum = maxval(self%values_2D_)
    case (3)
      maximum = maxval(self%values_3D_)
    case (4)
      maximum = maxval(self%values_4D_)
    case default
      error stop 'NetCDF_variable_s(default_real_maximum): unsupported rank'
    end select 
  end procedure

  module procedure double_precision_maximum
    select case(self%rank())
    case (1)
      maximum = maxval(self%values_1D_)
    case (2)
      maximum = maxval(self%values_2D_)
    case (3)
      maximum = maxval(self%values_3D_)
    case (4)
      maximum = maxval(self%values_4D_)
    case default
      error stop 'NetCDF_variable_s(double_precision_maximum): unsupported rank'
    end select 
  end procedure

  module procedure default_real_compare
    integer i
    print *, "Sizes = ", size(self%values_1D_), size(values) 
    do i=1, min(size(self%values_1D_), size(values))
      print *, self%values_1D_(i), values(i)%string()
    end do
  end procedure

  module procedure double_precision_compare
    integer i
    print *, "Sizes = ", size(self%values_1D_), size(values) 
    do i=1, min(size(self%values_1D_), size(values))
      print *, self%values_1D_(i), ", ", values(i)%string()
    end do
  end procedure

end submodule NetCDF_variable_s