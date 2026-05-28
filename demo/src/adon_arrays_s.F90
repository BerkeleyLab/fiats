! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

submodule(adon_arrays_m) adon_arrays_s
  implicit none

  type file_metadata_t
    integer unit_num
    integer, allocatable :: dims(:)
  end type

contains

  module procedure read_npy

    associate(file_metadata => open_npy(path // "/branch_input.npy"))
      allocate(adon_arrays%branch_input(file_metadata%dims(1), file_metadata%dims(2))) ! C/Python ordering
      read(file_metadata%unit_num) adon_arrays%branch_dot_trunk
      close(file_metadata%unit_num)
    end associate

    associate(file_metadata => open_npy(path // "/branch_output_10_21.npy"))
      allocate(adon_arrays%branch_output(file_metadata%dims(1), file_metadata%dims(2))) ! C/Python ordering
      read(file_metadata%unit_num) adon_arrays%branch_dot_trunk
      close(file_metadata%unit_num)
    end associate

    associate(file_metadata => open_npy(path // "/branch_dot_trunk_10_20.npy"))
      allocate(adon_arrays%branch_dot_trunk(file_metadata%dims(1), file_metadata%dims(2))) ! C/Python ordering
      read(file_metadata%unit_num) adon_arrays%branch_dot_trunk
      close(file_metadata%unit_num)
    end associate

    associate(file_metadata => open_npy(path // "/trunk_output_10_20_21.npy"))
      allocate(adon_arrays%trunk_output(file_metadata%dims(1), file_metadata%dims(2), file_metadata%dims(3))) ! C/Python ordering
      read(file_metadata%unit_num) adon_arrays%trunk_output
      close(file_metadata%unit_num)
    end associate

  contains

    function open_npy(filename) result(file_metadata_)
      character(len=*), intent(in) :: filename
      type(file_metadata_t) file_metadata_
      integer :: ios, header_len
      integer(kind=int8)  :: preamble(8)
      integer(kind=int16) :: header_len_i16
      character(len=:), allocatable :: header

      open(newunit=file_metadata_%unit_num, file=filename, access='stream', form='unformatted', &
           status='old', action='read', iostat=ios)
      if (ios /= 0) error stop "Cannot open .npy file"

      read(file_metadata_%unit_num) preamble
      read(file_metadata_%unit_num) header_len_i16
      header_len = int(header_len_i16)
      if (header_len < 0) header_len = header_len + 65536  ! treat as unsigned

      allocate(character(len=header_len) :: header)
      read(file_metadata_%unit_num) header

      file_metadata_%dims = parse_shape(header)
    end function

    pure function parse_shape(header) result(dims_)
      character(len=*), intent(in) :: header
      integer, allocatable :: dims_(:)

      integer :: p, p_open, p_close, i, j, n, ios, val
      character(len=len(header)) :: tuple_str
      character(len=32) :: token

      p = index(header, 'shape')
      if (p == 0) error stop "Cannot find 'shape' in .npy header"
      p_open  = index(header(p:), '(') + p - 1
      p_close = index(header(p:), ')') + p - 1
      tuple_str = header(p_open+1 : p_close-1)

      n = 0
      j = 1
      do i = 1, len_trim(tuple_str) + 1
        if (i > len_trim(tuple_str) .or. tuple_str(i:i) == ',') then
          if (len_trim(adjustl(tuple_str(j:i-1))) > 0) n = n + 1
          j = i + 1
        end if
      end do

      allocate(dims_(n))

      n = 0
      j = 1
      do i = 1, len_trim(tuple_str) + 1
        if (i > len_trim(tuple_str) .or. tuple_str(i:i) == ',') then
          token = adjustl(tuple_str(j:i-1))
          if (len_trim(token) > 0) then
            n = n + 1
            read(token, *, iostat=ios) val
            if (ios == 0) dims_(n) = val
          end if
          j = i + 1
        end if
      end do
    end function

  end procedure read_npy

end submodule adon_arrays_s
