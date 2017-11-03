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

#include "check.inc"

program errors_test
    use chemfiles
    use warnings

    implicit none
    integer :: status
    character(len=1024) :: error
    character(len=1024) :: version
    character(len=1024) :: expected
    type(chfl_trajectory) :: trajectory

    version = chfl_version()
    CHECK(len_trim(version) > 0)

    error = chfl_last_error()
    CHECK(trim(error) == '')

    call chfl_set_warning_callback(my_callback, status=status)
    CHECK(status == CHFL_SUCCESS)
    ! Generating an error message
    call trajectory%open("nothere", "r")
    expected = "file at 'nothere' does not have an extension, provide a format name to read it"
    CHECK(trim(last_message) == trim(expected))

    error = chfl_last_error()
    CHECK(trim(error) == trim(expected))

    call chfl_clear_errors(status=status)
    CHECK(status == CHFL_SUCCESS)
    error = chfl_last_error()
    CHECK(trim(error) == '')
end program
