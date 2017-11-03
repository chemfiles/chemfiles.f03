module warnings
    use chemfiles
    implicit none
    character(len=200) :: last_message
contains
    subroutine my_callback(message)
        implicit none
        character(len=*), intent(in) :: message

        last_message = message
    end subroutine my_callback
end module warnings

program errors_test
    use chemfiles
    use testing
    use warnings

    implicit none
    integer :: status
    character(len=1024) :: error
    character(len=1024) :: version
    type(chfl_trajectory) :: trajectory

    version = chfl_version()
    call check(len_trim(version) > 0)

    error = chfl_last_error()
    call check(trim(error) == "", "chfl_last_error")

    call chfl_set_warning_callback(my_callback, status=status)
    call check(status == CHFL_SUCCESS, "chfl_set_warning_callback")
    ! Generating an error message
    call trajectory%open("nothere", "r")
    call check( &
        trim(last_message) == "file at 'nothere' does not have an extension, provide a format name to read it", &
        "chfl_set_warning_callback" &
    )

    error = chfl_last_error()
    call check( &
        trim(error) == "file at 'nothere' does not have an extension, provide a format name to read it", &
        "chfl_last_error" &
    )

    call chfl_clear_errors(status=status)
    call check(status == CHFL_SUCCESS, "chfl_clear_errors")
    error = chfl_last_error()
    call check(trim(error) == '', "chfl_last_error")
end program
