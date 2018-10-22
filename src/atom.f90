! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_atom
    use iso_c_binding
    use chemfiles_cdef
    use chemfiles_strings
    implicit none

    private
    public :: chfl_atom

    type chfl_atom
        private
        type(c_ptr) :: ptr = c_null_ptr
    contains
        procedure :: unsafe_set_ptr
        procedure :: unsafe_ptr
        procedure :: unsafe_const_ptr

        generic, public :: init => create, copy
        procedure, private :: create, copy

        procedure :: mass
        procedure :: set_mass
        procedure :: charge
        procedure :: set_charge
        procedure :: type
        procedure :: set_type
        procedure :: name
        procedure :: set_name
        procedure :: full_name
        procedure :: vdw_radius
        procedure :: covalent_radius
        procedure :: atomic_number
        procedure :: free
        ! procedure :: properties_count => atom_properties_count
        ! procedure :: list_properties => atom_list_properties
        ! procedure :: set => atom_set_property
        ! procedure :: get => atom_get_property
    end type

contains
    subroutine unsafe_set_ptr(this, ptr, status)
        implicit none
        class(chfl_atom), intent(inout) :: this
        type(c_ptr), intent(in) :: ptr
        integer(chfl_status), optional, intent(out) :: status
        integer(chfl_status) :: dummy

        if (c_associated(this%unsafe_ptr())) then
            print*, "Trying to reset an allocated chfl_atom. Call chfl_atom%free first."
            ! free the allocated memory
            dummy = c_chfl_atom_free(ptr)
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
        class(chfl_atom), intent(inout) :: this
        unsafe_ptr = this%ptr
    end function

    type(c_ptr) function unsafe_const_ptr(this)
        implicit none
        class(chfl_atom), intent(in) :: this
        unsafe_const_ptr = this%ptr
    end function

    subroutine create(this, name, status)
        implicit none
        class(chfl_atom), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(c_chfl_atom(f_to_c_str(name)), status)
    end subroutine

    subroutine copy(this, atom, status)
        implicit none
        class(chfl_atom), intent(inout) :: this
        type(chfl_atom), intent(in) :: atom
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(c_chfl_atom_copy(atom%unsafe_const_ptr()), status)
    end subroutine

    real(kind=c_double) function mass(this, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_atom_mass(this%unsafe_const_ptr(), mass)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine set_mass(this, mass, status)
        implicit none
        class(chfl_atom), intent(inout) :: this
        real(kind=c_double), intent(in) :: mass
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_atom_set_mass(this%unsafe_ptr(), mass)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    real(kind=c_double) function charge(this, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_atom_charge(this%unsafe_const_ptr(), charge)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine set_charge(this, charge, status)
        implicit none
        class(chfl_atom), intent(inout) :: this
        real(kind=c_double), intent(in) :: charge
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_atom_set_charge(this%unsafe_ptr(), charge)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    character(len=CHFL_STRING_LENGTH) function type(this, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_atom_type(this%unsafe_const_ptr(), type, int(CHFL_STRING_LENGTH, c_int64_t))
        type = rm_null_in_str(type)
        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine set_type(this, type, status)
        implicit none
        class(chfl_atom), intent(inout) :: this
        character(len=*), intent(in) :: type
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_atom_set_type(this%unsafe_ptr(), f_to_c_str(type))

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    character(len=CHFL_STRING_LENGTH) function name(this, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_atom_name(this%unsafe_const_ptr(), name, int(CHFL_STRING_LENGTH, c_int64_t))
        name = rm_null_in_str(name)
        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine set_name(this, name, status)
        implicit none
        class(chfl_atom), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_atom_set_name(this%unsafe_ptr(), f_to_c_str(name))

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    function full_name(this, status) result(name)
        implicit none
        class(chfl_atom), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        character(len=CHFL_STRING_LENGTH) :: name
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_atom_full_name(this%unsafe_const_ptr(), name, int(CHFL_STRING_LENGTH, c_int64_t))
        name = rm_null_in_str(name)
        if (present(status)) then
            status = status_tmp
        end if
    end function

    real(kind=c_double) function vdw_radius(this, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_atom_vdw_radius(this%unsafe_const_ptr(), vdw_radius)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    real(kind=c_double) function covalent_radius(this, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_atom_covalent_radius(this%unsafe_const_ptr(), covalent_radius)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    integer(kind=c_int64_t) function atomic_number(this, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_atom_atomic_number(this%unsafe_const_ptr(), atomic_number)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    ! subroutine properties_count(this, count, status)
    !     implicit none
    !     class(chfl_atom), intent(in) :: this
    !     integer(kind=c_int64_t) :: count
    !     integer(chfl_status), intent(out), optional :: status
    !     integer(chfl_status) :: status_tmp
    !
    !     status_tmp = c_chfl_atom_properties_count(this%unsafe_ptr(), count)
    !
    !     if (present(status)) then
    !         status = status_tmp
    !     end if
    ! end subroutine

    ! subroutine list_properties(this, names, count, status)
    !     implicit none
    !     class(chfl_atom), intent(in) :: this
    !     character, intent(in), dimension(:, :), target :: names
    !     integer(kind=c_int64_t), intent(in) :: count
    !     integer(chfl_status), intent(out), optional :: status
    !     integer(chfl_status) :: status_tmp
    !
    !     status_tmp = c_chfl_atom_list_properties(this%unsafe_ptr(), c_loc(names), count)
    !
    !     if (present(status)) then
    !         status = status_tmp
    !     end if
    ! end subroutine

    ! subroutine set_property(this, name, property, status)
    !     implicit none
    !     class(chfl_atom), intent(inout) :: this
    !     character(len=*), intent(in) :: name
    !     class(chfl_property), intent(in) :: property
    !     integer(chfl_status), intent(out), optional :: status
    !     integer(chfl_status) :: status_tmp
    !
    !     status_tmp = c_chfl_atom_set_property(this%unsafe_ptr(), f_to_c_str(name), property%unsafe_ptr())
    !
    !     if (present(status)) then
    !         status = status_tmp
    !     end if
    ! end subroutine
    !
    ! subroutine get_property_init_(this, atom, name, status)
    !     implicit none
    !     class(chfl_property) :: this
    !     class(chfl_atom), intent(in) :: atom
    !     character(len=*), intent(in) :: name
    !     integer(chfl_status), intent(out), optional :: status
    !     integer(chfl_status) :: status_tmp
    !
    !     this%unsafe_ptr() = c_chfl_atom_get_property(atom%unsafe_ptr(), f_to_c_str(name))
    !
    !     if (.not. c_associated(this%unsafe_ptr())) then
    !         status_tmp = CHFL_MEMORY_ERROR
    !     else
    !         status_tmp = CHFL_SUCCESS
    !     end if
    !
    !
    !     if (present(status)) then
    !         status = status_tmp
    !     end if
    ! end subroutine

    subroutine free(this, status)
        implicit none
        class(chfl_atom), intent(inout) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_atom_free(this%unsafe_ptr())
        this%ptr = c_null_ptr

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine
end module
