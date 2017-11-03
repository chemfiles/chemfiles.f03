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


subroutine chfl_property_bool_init_(this, value, status)
    implicit none
    class(chfl_property) :: this
    logical(kind=c_bool), value :: value
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_property_bool(value)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_property_double_init_(this, value, status)
    implicit none
    class(chfl_property) :: this
    real(kind=c_double), value :: value
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_property_double(value)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_property_string_init_(this, value, status)
    implicit none
    class(chfl_property) :: this
    character(len=*), intent(in) :: value
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_property_string(f_to_c_str(value))

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_property_vector3d_init_(this, value, status)
    implicit none
    class(chfl_property) :: this
    real(kind=c_double), dimension(3), intent(in) :: value
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_property_vector3d(value)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_property_get_kind(this, kind, status)
    implicit none
    class(chfl_property), intent(in) :: this
    integer(chfl_property_kind) :: kind
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_property_get_kind(this%ptr, kind)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_property_get_bool(this, data, status)
    implicit none
    class(chfl_property), intent(in) :: this
    logical(kind=c_bool) :: data
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_property_get_bool(this%ptr, data)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_property_get_double(this, data, status)
    implicit none
    class(chfl_property), intent(in) :: this
    real(kind=c_double) :: data
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_property_get_double(this%ptr, data)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_property_get_string(this, buffer, buffsize, status)
    implicit none
    class(chfl_property), intent(in) :: this
    character(len=*) :: buffer
    integer(kind=c_int64_t), value :: buffsize
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_property_get_string(this%ptr, buffer, buffsize)
    buffer = rm_null_in_str(buffer)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_property_get_vector3d(this, data, status)
    implicit none
    class(chfl_property), intent(in) :: this
    real(kind=c_double), dimension(3) :: data
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_property_get_vector3d(this%ptr, data)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_property_free(this, status)
    implicit none
    class(chfl_property) :: this
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_property_free(this%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
    this%ptr = c_null_ptr
end subroutine
