! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(netCDF_file_m) netCDF_file_s
  use netcdf, only : &
     nf90_create, nf90_def_dim, nf90_def_var, nf90_enddef, nf90_put_var, nf90_inquire_dimension, & ! functions
     nf90_close, nf90_open, nf90_inq_varid, nf90_get_var, nf90_inquire_variable, &
     nf90_clobber, nf90_noerr, nf90_strerror, nf90_int, nf90_nowrite ! constants
  use julienne_m, only : operator(.equalsExpected.), julienne_assert, operator(//)
  implicit none

contains

  module procedure construct_from_string_name
   netCDF_file%file_name_ = file_name
  end procedure

  module procedure construct_from_character_name
   netCDF_file%file_name_ = file_name
  end procedure

  function get_shape(ncid, varname) result(array_shape)
    implicit none
    character(len=*), intent(in) :: varname
    integer, intent(in) :: ncid
    integer, allocatable :: array_shape(:)
    character(len=32) varid_string
    integer varid, dimlen, i, var_rank
    integer, parameter :: max_rank=15
    integer,dimension(max_rank+1) :: dims, dimIds
    associate(nf_status => nf90_inq_varid(ncid, varname, varid))
      write(varid_string, *) varid
      call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)) // ", varname '" // varname // "', varid " // trim(adjustl(varid_string)))
    end associate
    associate(nf_status => nf90_inquire_variable(ncid, varid, ndims = var_rank))
      call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)) // "(" // varname // ")")
        
    end associate
    associate(nf_status => nf90_inquire_variable(ncid, varid, dimids = dimIds(:var_rank)))
      call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)) // "(" // varname // ")")
    end associate
    do i=1,var_rank
      associate(nf_status => nf90_inquire_dimension(ncid, dimIds(i), len = dimlen))
        call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)) // "(" // varname // ")")
      end associate
      dims(i+1)=dimlen
    end do
    array_shape = dims(2:var_rank+1)
  end function 

  module procedure input_integer

    character(len=32) varid_string
    integer ncid, varid

    associate( nf_status => nf90_open(self%file_name_, nf90_nowrite, ncid) ) ! open file with read-only acces
      call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)) // self%file_name_)
    end associate

    associate( nf_status => nf90_inq_varid(ncid, varname, varid)) ! get variable's ID
      write(varid_string, *) varid
      call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)) // ", varname '" // varname // "', varid " // trim(adjustl(varid_string)))
    end associate

    select rank(values)
      rank(1)
        associate(array_shape => get_shape(ncid, varname))
          call julienne_assert(size(array_shape) .equalsExpected. rank(values))
          allocate(values(array_shape(1)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(2)
        associate(array_shape => get_shape(ncid, varname))
          call julienne_assert(size(array_shape) .equalsExpected. rank(values))
          allocate(values(array_shape(1), array_shape(2)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(3)
        associate(array_shape => get_shape(ncid, varname))
          call julienne_assert(size(array_shape) .equalsExpected. rank(values))
          allocate(values(array_shape(1), array_shape(2), array_shape(3)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(4)
        associate(array_shape => get_shape(ncid, varname))
          call julienne_assert(size(array_shape).equalsExpected. rank(values))
          allocate(values(array_shape(1), array_shape(2), array_shape(3), array_shape(4)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank default
        error stop "NetCDF_file_s(input_integer): unsupported rank"
    end select

  end procedure

  module procedure input_int64

    character(len=32) varid_string
    integer ncid, varid

    associate( nf_status => nf90_open(self%file_name_, nf90_nowrite, ncid) ) ! open file with read-only acces
      call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)) // self%file_name_)
    end associate

    associate( nf_status => nf90_inq_varid(ncid, varname, varid)) ! get variable's ID
      write(varid_string, *) varid
      call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)) // ", varname '" // varname // "', varid " // trim(adjustl(varid_string)))

    end associate

    select rank(values)
      rank(1)
        associate(array_shape => get_shape(ncid, varname))
          call julienne_assert(size(array_shape) .equalsExpected. rank(values))
          allocate(values(array_shape(1)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(2)
    print *,"---- rank(2) ----"
        associate(array_shape => get_shape(ncid, varname))
          call julienne_assert(size(array_shape) .equalsExpected. rank(values))
          allocate(values(array_shape(1), array_shape(2)))
    print *,"---- nf90_get_var(",ncid," ",varname," values) ----"
    print *,"---- shape(values), ",shape(values),"  ----"
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
    print *,"---- nf90_get_var done ----"
          call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(3)
        associate(array_shape => get_shape(ncid, varname))
          call julienne_assert(size(array_shape) .equalsExpected. rank(values))
          allocate(values(array_shape(1), array_shape(2), array_shape(3)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(4)
        associate(array_shape => get_shape(ncid, varname))
          call julienne_assert(size(array_shape).equalsExpected. rank(values))
          allocate(values(array_shape(1), array_shape(2), array_shape(3), array_shape(4)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank default
        error stop "NetCDF_file_s(input_int64): unsupported rank"
    end select

    print *,"---- input_int64 ----"

  end procedure

  module procedure input_double_precision
    character(len=32) varid_string
    integer ncid, varid

    associate( nf_status => nf90_open(self%file_name_, nf90_nowrite, ncid) ) ! open file with read-only acces
      call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)) // self%file_name_)
    end associate

    associate( nf_status => nf90_inq_varid(ncid, varname, varid)) ! get variable's ID
      write(varid_string, *) varid
      call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)) // "varname '" // varname // "', varid " // trim(adjustl(varid_string)))
    end associate

    select rank(values)
      rank(1)
        associate(array_shape => get_shape(ncid, varname))
          call julienne_assert(size(array_shape) .equalsExpected. rank(values))
          allocate(values(array_shape(1)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(2)
        associate(array_shape => get_shape(ncid, varname))
          call julienne_assert(size(array_shape) .equalsExpected. rank(values))
          allocate(values(array_shape(1),array_shape(2)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(3)
        associate(array_shape => get_shape(ncid, varname))
          call julienne_assert(size(array_shape) .equalsExpected. rank(values))
          allocate(values(array_shape(1),array_shape(2),array_shape(3)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(4)
        associate(array_shape => get_shape(ncid, varname))
          call julienne_assert(size(array_shape) .equalsExpected. rank(values))
          allocate(values(array_shape(1),array_shape(2),array_shape(3),array_shape(4)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank default
        error stop "NetCDF_file_s(input_double): unsupported rank"
    end select
  end procedure

  module procedure input_real

    character(len=32) varid_string
    integer ncid, varid

    associate( nf_status => nf90_open(self%file_name_, nf90_nowrite, ncid) ) ! open file with read-only acces
      call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)) // ", " // self%file_name_)
    end associate

    associate( nf_status => nf90_inq_varid(ncid, varname, varid)) ! get variable's ID
      write(varid_string, *) varid
      call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)) // ", varname '" // varname // "', varid " // trim(adjustl(varid_string)))
    end associate

    select rank(values)
      rank(0)
        allocate(values)
        associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
          call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
        end associate
      rank(1)
        associate(array_shape => get_shape(ncid, varname))
          call julienne_assert(size(array_shape) .equalsExpected. rank(values))
          allocate(values(array_shape(1)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(2)
        associate(array_shape => get_shape(ncid, varname))
          call julienne_assert(size(array_shape) .equalsExpected. rank(values))
          allocate(values(array_shape(1), array_shape(2)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(3)
        associate(array_shape => get_shape(ncid, varname))
          call julienne_assert(size(array_shape) .equalsExpected. rank(values))
          allocate(values(array_shape(1), array_shape(2), array_shape(3)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank(4)
        associate(array_shape => get_shape(ncid, varname))
          call julienne_assert(size(array_shape) .equalsExpected. rank(values))
          allocate(values(array_shape(1), array_shape(2), array_shape(3), array_shape(4)))
          associate( nf_status => nf90_get_var(ncid, varid, values)) ! read data
            call julienne_assert((nf_status .equalsExpected. nf90_noerr) // trim(nf90_strerror(nf_status)))
          end associate
        end associate
      rank default
        error stop "NetCDF_file_s(input_real_array): unsupported rank"
    end select

  end procedure

end submodule netCDF_file_s
