! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_selection
    use iso_c_binding
    use chemfiles_cdef
    use chemfiles_strings
    use chemfiles_frame
    implicit none

    private
    public :: chfl_selection

    type chfl_selection
        private
        type(c_ptr) :: ptr = c_null_ptr
    contains
        procedure :: unsafe_set_ptr
        procedure :: unsafe_const_ptr
        procedure :: unsafe_ptr

        generic :: init => create, copy
        procedure, private :: create, copy

        procedure :: size => get_size
        procedure :: string
        procedure :: evaluate
        procedure :: matches
        procedure :: free
    end type

contains
    subroutine unsafe_set_ptr(this, ptr, status)
        implicit none
        class(chfl_selection), intent(inout) :: this
        type(c_ptr), intent(in) :: ptr
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: dummy

        if (c_associated(this%unsafe_ptr())) then
            print*, "Trying to reset an allocated chfl_selection. Call chfl_selection%free first."
            ! free the allocated memory
            dummy = c_chfl_selection_free(ptr)
            if (present(status)) then
                status = CHFL_MEMORY_ERROR
            end if
            return
        end if

        this%ptr = ptr

        if (present(status)) then
            if (.not. c_associated(this%unsafe_ptr())) then
                status = CHFL_MEMORY_ERROR
            else
                status = CHFL_SUCCESS
            end if
        end if
    end subroutine

    type(c_ptr) function unsafe_ptr(this)
        implicit none
        class(chfl_selection), intent(inout) :: this
        unsafe_ptr = this%ptr
    end function

    type(c_ptr) function unsafe_const_ptr(this)
        implicit none
        class(chfl_selection), intent(in) :: this
        unsafe_const_ptr = this%ptr
    end function

    subroutine create(this, selection, status)
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

        status_tmp = c_chfl_selection_evaluate(this%unsafe_ptr(), frame%ptr, count)

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

    subroutine free(this, status)
        implicit none
        class(chfl_selection), intent(inout) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_selection_free(this%unsafe_const_ptr())
        this%ptr = c_null_ptr

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine
end module
