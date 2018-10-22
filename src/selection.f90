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
        procedure :: init => chfl_selection_init_
        procedure :: copy => chfl_selection_copy_init_
        procedure :: size => chfl_selection_size
        procedure :: string => chfl_selection_string
        procedure :: evaluate => chfl_selection_evaluate
        procedure :: matches => chfl_selection_matches
        procedure :: free => chfl_selection_free
    end type

contains
    subroutine chfl_selection_init_(this, selection, status)
        implicit none
        class(chfl_selection) :: this
        character(len=*), intent(in) :: selection
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        this%ptr = c_chfl_selection(f_to_c_str(selection))

        if (.not. c_associated(this%ptr)) then
            status_tmp_ = CHFL_MEMORY_ERROR
        else
            status_tmp_ = CHFL_SUCCESS
        end if


        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_selection_copy_init_(this, selection, status)
        implicit none
        class(chfl_selection) :: this
        class(chfl_selection), intent(in) :: selection
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        this%ptr = c_chfl_selection_copy(selection%ptr)

        if (.not. c_associated(this%ptr)) then
            status_tmp_ = CHFL_MEMORY_ERROR
        else
            status_tmp_ = CHFL_SUCCESS
        end if


        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_selection_size(this, size, status)
        implicit none
        class(chfl_selection), intent(in) :: this
        integer(kind=c_int64_t) :: size
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_selection_size(this%ptr, size)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_selection_string(this, string, buffsize, status)
        implicit none
        class(chfl_selection), intent(in) :: this
        character(len=*) :: string
        integer(kind=c_int64_t), value :: buffsize
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_selection_string(this%ptr, string, buffsize)
        string = rm_null_in_str(string)
        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_selection_evaluate(this, frame, n_matches, status)
        implicit none
        class(chfl_selection) :: this
        class(chfl_frame), intent(in) :: frame
        integer(kind=c_int64_t) :: n_matches
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_selection_evaluate(this%ptr, frame%ptr, n_matches)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_selection_matches(this, matches, count, status)
        implicit none
        class(chfl_selection), intent(in) :: this
        integer(kind=c_int64_t), value :: count
        type(chfl_match), dimension(count), target :: matches
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_selection_matches(this%ptr, c_loc(matches), count)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_selection_free(this, status)
        implicit none
        class(chfl_selection), intent(in) :: this
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_selection_free(this%ptr)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine
end module
