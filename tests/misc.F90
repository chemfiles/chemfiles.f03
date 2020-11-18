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

program misc_test
    use chemfiles
    use warnings

    implicit none
    integer :: status
    character(len=1024) :: error
    character(len=1024) :: version
    character(len=1024) :: expected
    type(chfl_format_metadata), dimension(:), allocatable :: metadata
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

    call chfl_add_configuration("data/config.toml", status=status)
    CHECK(status == CHFL_SUCCESS)

    call chfl_formats_list(metadata, status=status)
    CHECK(status == CHFL_SUCCESS)

    CHECK(size(metadata) == 21)
    CHECK(trim(metadata(21)%name) == 'XYZ')
    CHECK(trim(metadata(21)%extension) == '.xyz')
    CHECK(trim(metadata(21)%description) == 'XYZ text format')
    CHECK(trim(metadata(21)%reference) == 'https://openbabel.org/wiki/XYZ')

    CHECK(metadata(21)%read .eqv. .true.)
    CHECK(metadata(21)%write .eqv. .true.)
    CHECK(metadata(21)%memory .eqv. .true.)
    CHECK(metadata(21)%positions .eqv. .true.)
    CHECK(metadata(21)%velocities .eqv. .false.)
    CHECK(metadata(21)%unit_cell .eqv. .true.)
    CHECK(metadata(21)%atoms .eqv. .true.)
    CHECK(metadata(21)%bonds .eqv. .false.)
    CHECK(metadata(21)%residues .eqv. .false.)

    deallocate(metadata)
end program
