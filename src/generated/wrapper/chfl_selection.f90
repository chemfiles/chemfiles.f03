! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2017 Guillaume Fraux -- BSD licence
!
! =========================================================================== !
! !!!! AUTO-GENERATED FILE !!!! Do not edit. See bindgen repository for the
! generating code (https://github.com/chemfiles/bindgen).
! This file contains Fortran 2003 ISO C Binding interface to the C API
!
! This file is not compilable on his own, but should be 'include'd in another
! fortran compilation unit.
! =========================================================================== !


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

subroutine chfl_selection_matches(this, matches, n_matches, status)
    implicit none
    class(chfl_selection), intent(in) :: this
    type(chfl_match), dimension(:), target :: matches
    integer(kind=c_int64_t), value :: n_matches
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_selection_matches(this%ptr, c_loc(matches), n_matches)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_selection_free(this, status)
    implicit none
    class(chfl_selection) :: this
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_selection_free(this%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
    this%ptr = c_null_ptr
end subroutine
