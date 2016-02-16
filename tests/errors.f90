module logging
    use chemfiles
    implicit none
    character(len=200) :: last_message
    integer(kind=kind(CHFL_LOG_LEVEL)) :: last_level
contains
    subroutine my_callback(level, message)
        implicit none
        integer(kind=kind(CHFL_LOG_LEVEL)), intent(in) :: level
        character(len=*), intent(in) :: message

        last_level = level
        last_message = message
    end subroutine my_callback
end module logging

program cell_test
    use chemfiles
    use testing
    use logging

    implicit none
    integer(kind=kind(CHFL_LOG_LEVEL)) :: level
    integer :: status
    logical :: fexist
    character(len=1024) :: error
    type(chfl_trajectory) :: trajectory

    error = chfl_strerror(int(0, 4))
    call check((trim(error) == "Operation was sucessfull"), "chfl_strerror")

    error = chfl_last_error()
    call check((trim(error) == ""), "chfl_last_error")

    call chfl_loglevel(level, status=status)
    call check((status == 0), "chfl_loglevel")
    call check((level == CHFL_LOG_WARNING), "chfl_loglevel")

    call chfl_set_loglevel(CHFL_LOG_DEBUG, status=status)
    call check((status == 0), "chfl_set_loglevel")

    call chfl_loglevel(level, status=status)
    call check((status == 0), "chfl_loglevel")
    call check((level == CHFL_LOG_DEBUG), "chfl_loglevel")

    call chfl_logfile("test.log", status=status)
    call check((status == 0), "chfl_logfile")

    inquire(file="test.log", exist=fexist)
    call check(fexist, "File created by chfl_logfile")

    call chfl_log_callback(my_callback, status=status)
    call check((status == 0), "chfl_log_callback")
    ! Generating an error message
    call trajectory%open("nothere", "r")
    call check(last_level == CHFL_LOG_ERROR, "chfl_log_callback level")
    call check(trim(last_message) == 'Can not find a format associated with the "" extension.', "chfl_log_callback message")

    call chfl_log_stderr(status=status)
    call check((status == 0), "chfl_log_stderr")

    open(unit=11, iostat=status, file="test.log", status='old')
    if (status == 0) close(11, status='delete')
end program
