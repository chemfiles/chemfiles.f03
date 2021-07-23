! chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux - BSD License

module chemfiles_misc
    use iso_c_binding
    use chemfiles_ffi
    use chemfiles_strings

    implicit none

    private
    public :: chfl_last_error, chfl_clear_errors
    public :: chfl_set_warning_callback
    public :: chfl_add_configuration
    public :: chfl_formats_list, chfl_format_metadata, chfl_guess_format

    ! Global pointer to the callback procedure
    procedure(chfl_warning_callback), pointer :: internal_warning_callback

    interface
        subroutine chfl_warning_callback(message)
            implicit none
            character(len=*), intent(in) :: message
        end subroutine chfl_warning_callback
    end interface

    type :: chfl_format_metadata
        character(len=CHFL_STRING_LENGTH) :: name
        character(len=CHFL_STRING_LENGTH) :: extension
        character(len=CHFL_STRING_LENGTH) :: description
        character(len=CHFL_STRING_LENGTH) :: reference

        logical :: read
        logical :: write
        logical :: memory

        logical :: positions
        logical :: velocities
        logical :: unit_cell
        logical :: atoms
        logical :: bonds
        logical :: residues
    end type

contains
    function chfl_last_error() result(string)
        implicit none

        character(len=CHFL_STRING_LENGTH) :: string
        type(c_ptr) :: c_string

        c_string = c_chfl_last_error()
        string = c_to_f_str(c_string)
    end function

    subroutine chfl_clear_errors(status)
        implicit none
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_clear_errors()

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_add_configuration(path, status)
        implicit none
        character(len=*), intent(in) :: path
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_add_configuration(f_to_c_str(path))

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_formats_list(metadata,status)
        type(chfl_format_metadata), dimension(:), allocatable, intent(inout) :: metadata
        integer, optional :: status

        type(c_ptr), target :: ptr
        integer(kind=c_int64_t), target :: count
        integer(kind=c_int64_t) :: i
        type(c_chfl_format_metadata), dimension(:), pointer :: c_metadata

        integer :: status_tmp_

        if (allocated(metadata)) then
            deallocate(metadata)
        end if

        status_tmp_ = c_chfl_formats_list(c_loc(ptr), count)
        if (present(status)) then
            status = status_tmp_
        end if

        if (status_tmp_ /= CHFL_SUCCESS) then
            return
        end if

        call c_f_pointer(ptr, c_metadata, [count])
        allocate(metadata(count))
        do i=1,count
            metadata(i)%name = c_to_f_str(c_metadata(i)%name)
            if (c_associated(c_metadata(i)%extension)) then
                metadata(i)%extension = c_to_f_str(c_metadata(i)%extension)
            endif
            metadata(i)%description = c_to_f_str(c_metadata(i)%description)
            metadata(i)%reference = c_to_f_str(c_metadata(i)%reference)

            metadata(i)%read = c_metadata(i)%read
            metadata(i)%write = c_metadata(i)%write
            metadata(i)%memory = c_metadata(i)%memory
            metadata(i)%positions = c_metadata(i)%positions
            metadata(i)%velocities = c_metadata(i)%velocities
            metadata(i)%unit_cell = c_metadata(i)%unit_cell
            metadata(i)%atoms = c_metadata(i)%atoms
            metadata(i)%bonds = c_metadata(i)%bonds
            metadata(i)%residues = c_metadata(i)%residues
        end do

        call c_chfl_free(ptr)
    end subroutine

    subroutine internal_warning_wrapper(message) bind(C)
        implicit none
        type(c_ptr), intent(in), value :: message
        call internal_warning_callback(c_to_f_str(message))
    end subroutine

    subroutine chfl_set_warning_callback(callback, status)
        implicit none
        procedure(chfl_warning_callback) :: callback
        integer, optional :: status
        integer :: status_tmp_

        internal_warning_callback => callback
        status_tmp_ = c_chfl_set_warning_callback(c_funloc(internal_warning_wrapper))
        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    function chfl_guess_format(path, status)
        implicit none
        character(len=64) :: chfl_guess_format
        character(len=*), intent(in) :: path
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_guess_format(path, chfl_guess_format, int(64, c_int64_t))
        chfl_guess_format = rm_null_in_str(chfl_guess_format)
        if (present(status)) then
            status = status_tmp
        end if
    end function
end module
