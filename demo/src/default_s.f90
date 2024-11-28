submodule(default_m) default_s
  implicit none

contains

  module procedure default_integer_or_present_value
    
    if (present(optional_value)) then
      set_value = optional_value
    else
      set_value = default_value
    end if

  end procedure

  module procedure default_real_or_present_value
    
    if (present(optional_value)) then
      set_value = optional_value
    else
      set_value = default_value
    end if

  end procedure

  module procedure default_integer_or_internal_read
    
    if (len(string)/=0) then
      read(string,*) set_value
    else
      set_value = default_value
    end if

  end procedure

  module procedure default_real_or_internal_read
    
    if (len(string)/=0) then
      read(string,*) set_value
    else
      set_value = default_value
    end if

  end procedure

end submodule default_s