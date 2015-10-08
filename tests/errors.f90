program cell_test
    use chemfiles
    use testing

    implicit none
    integer(kind=kind(CHFL_LOG_LEVEL)) :: level
    integer :: status
    logical :: fexist
    character(len=1024) :: error

    error = chfl_strerror(int(0, 4))
    call check((trim(error) == "Operation was sucessfull"), "chfl_strerror")

    error = chfl_last_error()
    call check((trim(error) == ""), "chfl_last_error")

    call chfl_loglevel(level, status=status)
    call check((status == 0), "chfl_loglevel")
    call check((level == CHFL_LOG_WARNING), "chfl_loglevel")

    call chfl_set_loglevel(CHFL_LOG_NONE, status=status)
    call check((status == 0), "chfl_set_loglevel")

    call chfl_loglevel(level, status=status)
    call check((status == 0), "chfl_loglevel")
    call check((level == CHFL_LOG_NONE), "chfl_loglevel")

    call chfl_logfile("test.log", status=status)
    call check((status == 0), "chfl_logfile")

    inquire(file="test.log", exist=fexist)
    call check(fexist, "File created by chfl_logfile")

    call chfl_log_stderr(status=status)
    call check((status == 0), "chfl_log_stderr")

    open(unit=11, iostat=status, file="test.log", status='old')
    if (status == 0) close(11, status='delete')
end program
