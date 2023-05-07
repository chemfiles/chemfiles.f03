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
    character(len=64) :: format
    integer :: xyz_index
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

    CHECK(size(metadata) == 24)
    xyz_index = 24
    CHECK(trim(metadata(xyz_index)%name) == 'XYZ')
    CHECK(trim(metadata(xyz_index)%extension) == '.xyz')
    CHECK(trim(metadata(xyz_index)%description) == 'XYZ text format')
    CHECK(trim(metadata(xyz_index)%reference) == 'https://openbabel.org/wiki/XYZ')

    CHECK(metadata(xyz_index)%read .eqv. .true.)
    CHECK(metadata(xyz_index)%write .eqv. .true.)
    CHECK(metadata(xyz_index)%memory .eqv. .true.)
    CHECK(metadata(xyz_index)%positions .eqv. .true.)
    CHECK(metadata(xyz_index)%velocities .eqv. .false.)
    CHECK(metadata(xyz_index)%unit_cell .eqv. .true.)
    CHECK(metadata(xyz_index)%atoms .eqv. .true.)
    CHECK(metadata(xyz_index)%bonds .eqv. .false.)
    CHECK(metadata(xyz_index)%residues .eqv. .false.)

    deallocate(metadata)

    format = chfl_guess_format("file.xyz.gz", status=status)
    CHECK(status == CHFL_SUCCESS)
    CHECK(trim(format) == 'XYZ / GZ')

    format = chfl_guess_format("file.nc", status=status)
    CHECK(status == CHFL_SUCCESS)
    CHECK(trim(format) == 'Amber NetCDF')
end program
