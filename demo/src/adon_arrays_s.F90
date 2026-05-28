! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

submodule(adon_arrays_m) adon_arrays_s
  implicit none

contains

  module procedure read_npy

    integer :: unit_num
    integer, allocatable :: dims(:)

    ! Dimensions reversed from numpy (C order) so a direct stream read is correct.
    ! numpy branch_dot_trunk shape (10, 20)  -> Fortran (20, 10)
    ! numpy trunk_output      shape (10,20,21)-> Fortran (21, 20, 10)

    call open_npy(path // "/branch_dot_trunk_10_20.npy", unit_num, dims)
    allocate(adon_arrays%branch_dot_trunk(dims(2), dims(1)))
    read(unit_num) adon_arrays%branch_dot_trunk
    close(unit_num)

    deallocate(dims)

    call open_npy(path // "/trunk_output_10_20_21.npy", unit_num, dims)
    allocate(adon_arrays%trunk_output(dims(3), dims(2), dims(1)))
    read(unit_num) adon_arrays%trunk_output
    close(unit_num)

  contains

    subroutine open_npy(filename, unit_num, dims)
      character(len=*), intent(in) :: filename
      integer, intent(out) :: unit_num
      integer, allocatable, intent(out) :: dims(:)

      integer :: ios, header_len
      integer(kind=int8)  :: preamble(8)
      integer(kind=int16) :: header_len_i16
      character(len=:), allocatable :: header

      open(newunit=unit_num, file=filename, access='stream', form='unformatted', &
           status='old', action='read', iostat=ios)
      if (ios /= 0) error stop "Cannot open .npy file"

      read(unit_num) preamble
      read(unit_num) header_len_i16
      header_len = int(header_len_i16)
      if (header_len < 0) header_len = header_len + 65536  ! treat as unsigned

      allocate(character(len=header_len) :: header)
      read(unit_num) header

      call parse_shape(header, dims)
    end subroutine

    subroutine parse_shape(header, dims)
      character(len=*), intent(in) :: header
      integer, allocatable, intent(out) :: dims(:)

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

      allocate(dims(n))

      n = 0
      j = 1
      do i = 1, len_trim(tuple_str) + 1
        if (i > len_trim(tuple_str) .or. tuple_str(i:i) == ',') then
          token = adjustl(tuple_str(j:i-1))
          if (len_trim(token) > 0) then
            n = n + 1
            read(token, *, iostat=ios) val
            if (ios == 0) dims(n) = val
          end if
          j = i + 1
        end if
      end do
    end subroutine

  end procedure

end submodule adon_arrays_s
