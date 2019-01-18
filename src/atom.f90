! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_atom
    use iso_c_binding
    use chemfiles_ffi
    use chemfiles_strings
    use chemfiles_property
    implicit none

    private
    public :: chfl_atom

    type, extends(chfl_ptr) :: chfl_atom
    contains
        procedure :: init
        procedure :: copy
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

        procedure :: properties_count
        procedure :: list_properties
        procedure :: get
        generic :: set => set_property, set_bool, set_double, set_string, set_vector3d
        procedure, private :: set_property
        procedure, private :: set_bool
        procedure, private :: set_double
        procedure, private :: set_string
        procedure, private :: set_vector3d
    end type

contains
    subroutine init(this, name, status)
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

    subroutine set_property(this, name, property, status)
        implicit none
        class(chfl_atom), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(chfl_property), intent(in) :: property
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_atom_set_property(                                &
            this%unsafe_ptr(), f_to_c_str(name), property%unsafe_const_ptr()  &
        )

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine set_bool(this, name, value, status)
        implicit none
        class(chfl_atom), intent(inout) :: this
        character(len=*), intent(in) :: name
        logical, intent(in) :: value
        integer(chfl_status), intent(out), optional :: status

        type(chfl_property) :: property
        integer(chfl_status) :: status_tmp

        call property%init(value, status=status_tmp)
        if (status_tmp /= CHFL_SUCCESS) then
            if (present(status)) then
                status = status_tmp
            end if
            return
        end if

        call this%set_property(name, property, status=status)
        call property%free()
    end subroutine

    subroutine set_double(this, name, value, status)
        implicit none
        class(chfl_atom), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(c_double), intent(in) :: value
        integer(chfl_status), intent(out), optional :: status

        type(chfl_property) :: property
        integer(chfl_status) :: status_tmp

        call property%init(value, status=status_tmp)
        if (status_tmp /= CHFL_SUCCESS) then
            if (present(status)) then
                status = status_tmp
            end if
            return
        end if

        call this%set_property(name, property, status=status)
        call property%free()
    end subroutine

    subroutine set_string(this, name, value, status)
        implicit none
        class(chfl_atom), intent(inout) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: value
        integer(chfl_status), intent(out), optional :: status

        type(chfl_property) :: property
        integer(chfl_status) :: status_tmp

        call property%init(value, status=status_tmp)
        if (status_tmp /= CHFL_SUCCESS) then
            if (present(status)) then
                status = status_tmp
            end if
            return
        end if

        call this%set_property(name, property, status=status)
        call property%free()
    end subroutine

    subroutine set_vector3d(this, name, value, status)
        implicit none
        class(chfl_atom), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(c_double), dimension(3), intent(in) :: value
        integer(chfl_status), intent(out), optional :: status

        type(chfl_property) :: property
        integer(chfl_status) :: status_tmp

        call property%init(value, status=status_tmp)
        if (status_tmp /= CHFL_SUCCESS) then
            if (present(status)) then
                status = status_tmp
            end if
            return
        end if

        call this%set_property(name, property, status=status)
        call property%free()
    end subroutine

    type(chfl_property) function get(this, name, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        character(len=*), intent(in) :: name
        integer(chfl_status), intent(out), optional :: status

        call get%unsafe_set_ptr(                                                 &
            c_chfl_atom_get_property(this%unsafe_const_ptr(), f_to_c_str(name)), &
            status                                                               &
        )
    end function

    integer(kind=c_int64_t) function properties_count(this, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_atom_properties_count(this%unsafe_const_ptr(), properties_count)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine list_properties(this, names, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        character(len=CHFL_STRING_LENGTH), intent(out), dimension(:) :: names
        integer(chfl_status), intent(out), optional :: status

        type(c_ptr), dimension(:), allocatable, target :: c_names
        integer(kind=c_int64_t) :: count, i
        integer(chfl_status) :: status_tmp

        count = size(names, 1, c_int64_t)
        allocate(c_names(count))

        status_tmp = c_chfl_atom_list_properties(this%unsafe_const_ptr(), c_loc(c_names), count)
        if (status_tmp /= CHFL_SUCCESS) goto 1000

        do i = 1, count
            names(i) = c_to_f_str(c_names(i))
        end do

1000    deallocate(c_names)
        if (present(status)) then
            status = status_tmp
        end if
    end subroutine
end module
