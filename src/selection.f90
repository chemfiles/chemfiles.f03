! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_selection
    use iso_c_binding
    use chemfiles_ffi
    use chemfiles_strings
    use chemfiles_frame
    implicit none

    private
    public :: chfl_selection

    type, extends(chfl_ptr) :: chfl_selection
    contains
        procedure :: init
        procedure :: copy
        procedure :: size => get_size
        procedure :: string
        procedure :: evaluate
        procedure :: matches
    end type

contains
    subroutine init(this, selection, status)
        implicit none
        class(chfl_selection), intent(inout) :: this
        character(len=*), intent(in) :: selection
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(c_chfl_selection(f_to_c_str(selection)), status)
    end subroutine

    subroutine copy(this, selection, status)
        implicit none
        class(chfl_selection), intent(inout) :: this
        class(chfl_selection), intent(in) :: selection
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(c_chfl_selection_copy(selection%unsafe_const_ptr()), status)
    end subroutine

    integer(kind=c_int64_t) function get_size(this, status)
        implicit none
        class(chfl_selection), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_selection_size(this%unsafe_const_ptr(), get_size)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    character(len=CHFL_STRING_LENGTH) function string(this, status)
        implicit none
        class(chfl_selection), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_selection_string(                                 &
            this%unsafe_const_ptr(), string, int(CHFL_STRING_LENGTH, c_int64_t)   &
        )
        string = rm_null_in_str(string)
        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine evaluate(this, frame, count, status)
        implicit none
        class(chfl_selection), intent(inout) :: this
        class(chfl_frame), intent(in) :: frame
        integer(kind=c_int64_t), intent(out) :: count
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_selection_evaluate(this%unsafe_ptr(), frame%unsafe_const_ptr(), count)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine matches(this, data, status)
        implicit none
        class(chfl_selection), intent(in) :: this
        type(chfl_match), intent(out), dimension(:), target :: data
        integer(chfl_status), intent(out), optional :: status
        integer(kind=c_int64_t) :: count
        integer(chfl_status) :: status_tmp

        count = size(data, 1, c_int64_t)
        status_tmp = c_chfl_selection_matches(this%unsafe_const_ptr(), c_loc(data), count)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine
end module
