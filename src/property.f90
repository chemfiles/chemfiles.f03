! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_property
    use iso_c_binding
    use chemfiles_ffi
    use chemfiles_strings
    implicit none

    private
    public :: chfl_property

    type, extends(chfl_ptr) :: chfl_property
    contains
        generic :: init => bool_init, double_init, string_init, vector3d_init
        procedure, private :: bool_init, double_init, string_init, vector3d_init

        procedure :: kind => get_kind
        procedure :: bool
        procedure :: double
        procedure :: string
        procedure :: vector3d
    end type

contains
    subroutine bool_init(this, value, status)
        implicit none
        class(chfl_property), intent(out) :: this
        logical, intent(in) :: value
        integer(chfl_status), optional, intent(out) :: status

        call this%unsafe_set_ptr(c_chfl_property_bool(logical(value, c_bool)), status)
    end subroutine

    subroutine double_init(this, value, status)
        implicit none
        class(chfl_property), intent(inout) :: this
        real(kind=c_double), intent(in) :: value
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(c_chfl_property_double(value), status)
    end subroutine

    subroutine string_init(this, value, status)
        implicit none
        class(chfl_property), intent(inout) :: this
        character(len=*), intent(in) :: value
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(c_chfl_property_string(f_to_c_str(value)), status)
    end subroutine

    subroutine vector3d_init(this, value, status)
        implicit none
        class(chfl_property), intent(inout) :: this
        real(kind=c_double), dimension(3), intent(in) :: value
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(c_chfl_property_vector3d(value), status)
    end subroutine

    function get_kind(this, status)
        implicit none
        class(chfl_property), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_property_kind) :: get_kind
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_property_get_kind(this%unsafe_const_ptr(), get_kind)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    logical function bool(this, status)
        implicit none
        class(chfl_property), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status

        integer(chfl_status) :: status_tmp
        logical(c_bool) :: value_tmp

        status_tmp = c_chfl_property_get_bool(this%unsafe_const_ptr(), value_tmp)
        bool = logical(value_tmp)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    real(kind=c_double) function double(this, status)
        implicit none
        class(chfl_property), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_property_get_double(this%unsafe_const_ptr(), double)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    character(len=CHFL_STRING_LENGTH) function string(this, status)
        implicit none
        class(chfl_property), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_property_get_string(this%unsafe_const_ptr(), string, int(CHFL_STRING_LENGTH, c_int64_t))
        string = rm_null_in_str(string)
        if (present(status)) then
            status = status_tmp
        end if
    end function

    function vector3d(this, status)
        implicit none
        class(chfl_property), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        real(kind=c_double), dimension(3) :: vector3d
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_property_get_vector3d(this%unsafe_const_ptr(), vector3d)

        if (present(status)) then
            status = status_tmp
        end if
    end function
end module
