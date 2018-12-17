! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_atom
    use iso_c_binding
    use chemfiles_cdef
    use chemfiles_strings
    use chemfiles_property
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
        procedure :: free

        generic :: set => set_property, set_bool, set_double, set_string, set_vector3d
        procedure, private :: set_property, set_bool, set_double, set_string, set_vector3d
        procedure :: properties_count
        procedure :: list_properties
        procedure :: get
    end type

contains
    subroutine unsafe_set_ptr(this, ptr, status)
        implicit none
        class(chfl_atom), intent(inout) :: this
        type(c_ptr), intent(in) :: ptr
        integer(chfl_status), optional, intent(out) :: status
        integer(chfl_status) :: dummy

        if (c_associated(this%ptr)) then
            write(*, *) "Trying to reset an allocated chfl_atom. Call chfl_atom%free first."
            ! free the allocated memory
            dummy = c_chfl_atom_free(ptr)
            if (present(status)) then
                status = CHFL_MEMORY_ERROR
            end if
            return
        end if

        this%ptr = ptr

        if (present(status)) then
            if (.not. c_associated(this%ptr)) then
                status = CHFL_MEMORY_ERROR
            else
                status = CHFL_SUCCESS
            end if
        end if
    end subroutine

    type(c_ptr) function unsafe_ptr(this)
        implicit none
        class(chfl_atom), intent(inout) :: this

        if (.not. c_associated(this%ptr)) then
            write(*, *) "Trying to access a NULL chfl_atom. Call chfl_atom%init first."
            stop 1
        end if
        unsafe_ptr = this%ptr
    end function

    type(c_ptr) function unsafe_const_ptr(this)
        implicit none
        class(chfl_atom), intent(in) :: this

        if (.not. c_associated(this%ptr)) then
            write(*, *) "Trying to access a NULL chfl_atom. Call chfl_atom%init first."
            stop 1
        end if
        unsafe_const_ptr = this%ptr
    end function

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
        if (status_tmp /= CHFL_SUCCESS) goto 1000

        call this%set_property(name, property, status=status)

        call property%free(status=status_tmp)
        if (status_tmp /= CHFL_SUCCESS) goto 1000

1000    if (present(status)) then
            status = status_tmp
        end if
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
        if (status_tmp /= CHFL_SUCCESS) goto 2000

        call this%set_property(name, property, status=status)

        call property%free(status=status_tmp)
        if (status_tmp /= CHFL_SUCCESS) goto 2000

2000    if (present(status)) then
            status = status_tmp
        end if
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
        if (status_tmp /= CHFL_SUCCESS) goto 3000

        call this%set_property(name, property, status=status)

        call property%free(status=status_tmp)
        if (status_tmp /= CHFL_SUCCESS) goto 3000

3000    if (present(status)) then
            status = status_tmp
        end if
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
        if (status_tmp /= CHFL_SUCCESS) goto 4000

        call this%set_property(name, property, status=status)

        call property%free(status=status_tmp)
        if (status_tmp /= CHFL_SUCCESS) goto 4000

4000    if (present(status)) then
            status = status_tmp
        end if
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
        if (status_tmp /= CHFL_SUCCESS) goto 5000

        do i = 1, count
            names(i) = c_to_f_str(c_names(i))
        end do

5000    deallocate(c_names)
        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine free(this, status)
        implicit none
        class(chfl_atom), intent(inout) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_atom_free(this%ptr)
        this%ptr = c_null_ptr

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine
end module
