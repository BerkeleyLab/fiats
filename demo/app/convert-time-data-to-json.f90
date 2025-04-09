! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
program convert_time_data_to_json
  !! Convert icar time-step output file to JSON format
  use julienne_m, only : string_t, file_t, command_line_t
  use time_data_m, only : time_data_t, icar_output_file_t
  implicit none

  character(len=*), parameter :: usage_info = &
    new_line('') // new_line('') // &
    'Usage:  ./build/run-fpm.sh run convert-time-data-to-json --  --input-file <string> --output-file <string>' // new_line('') // &
    'where angular brackets (<>) denote user-provided input.' // &
    new_line('')

  associate(time_data => time_data_t(icar_output_file_t(file_t(file_path("--input-file")))))
    associate(json_file => time_data%to_json())
      call json_file%write_lines(file_path("--output-file"))
    end associate
  end associate

  print *,new_line('') // "______ convert-time-data-to-json done _______"

contains

  function file_path(flag) result(string)
    character(len=*), intent(in) :: flag
    type(string_t) string
    character(len=:), allocatable :: flag_value
    type(command_line_t) command_line

    flag_value = command_line%flag_value(flag)
    if (len(flag_value)==0) error stop usage_info
    string = string_t(flag_value)
  end function

end program
