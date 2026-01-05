module hyperparameters_m
  use julienne_string_m, only : string_t
  implicit none

  type hyperparameters_t(k)
    integer, kind :: k = kind(1.)
    integer :: mini_batches_ = 10
  contains
    procedure to_json
  end type

  interface hyperparameters_t
    module procedure default_real_from_json
  end interface

  character(len=*), parameter :: mini_batches_key  = "mini-batches"

contains

  pure function default_real_from_json(lines) result(hyperparameters)
    type(string_t), intent(in) :: lines(:)
    type(hyperparameters_t) hyperparameters
    integer l

    do l=1,size(lines)
      if (lines(l)%get_json_key() == "hyperparameters") then
        hyperparameters%mini_batches_  = lines(l+1)%get_json_value(string_t(mini_batches_key), mold=0)
        return
      end if
    end do
  end function

  pure function to_json(self) result(lines)
    class(hyperparameters_t), intent(in) :: self
    type(string_t), allocatable :: lines(:)
    integer, parameter :: max_width= 18
    character(len=max_width) mini_batches_string

    write(mini_batches_string,*) self%mini_batches_
    lines = [string_t('"hyperparameters": {"' // mini_batches_key  // '" : '  // trim(adjustl(mini_batches_string)) // '}')]
  end function

end module
