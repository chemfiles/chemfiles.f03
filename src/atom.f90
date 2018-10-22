! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_atom
    use iso_c_binding
    use chemfiles_cdef
    use chemfiles_strings
    implicit none

    type chfl_atom
        type(c_ptr) :: ptr = c_null_ptr
    contains
        procedure :: init => chfl_atom_init_
        procedure :: copy => chfl_atom_copy_init_
        procedure :: mass => chfl_atom_mass
        procedure :: set_mass => chfl_atom_set_mass
        procedure :: charge => chfl_atom_charge
        procedure :: set_charge => chfl_atom_set_charge
        procedure :: type => chfl_atom_type
        procedure :: set_type => chfl_atom_set_type
        procedure :: name => chfl_atom_name
        procedure :: set_name => chfl_atom_set_name
        procedure :: full_name => chfl_atom_full_name
        procedure :: vdw_radius => chfl_atom_vdw_radius
        procedure :: covalent_radius => chfl_atom_covalent_radius
        procedure :: atomic_number => chfl_atom_atomic_number
        procedure :: free => chfl_atom_free
        ! procedure :: properties_count => chfl_atom_properties_count
        ! procedure :: list_properties => chfl_atom_list_properties
        ! procedure :: set => chfl_atom_set_property
        ! procedure :: get => chfl_atom_get_property
    end type


contains
    subroutine chfl_atom_init_(this, name, status)
        implicit none
        class(chfl_atom) :: this
        character(len=*), intent(in) :: name
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        this%ptr = c_chfl_atom(f_to_c_str(name))

        if (.not. c_associated(this%ptr)) then
            status_tmp_ = CHFL_MEMORY_ERROR
        else
            status_tmp_ = CHFL_SUCCESS
        end if


        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_atom_copy_init_(this, atom, status)
        implicit none
        class(chfl_atom) :: this
        class(chfl_atom), intent(in) :: atom
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        this%ptr = c_chfl_atom_copy(atom%ptr)

        if (.not. c_associated(this%ptr)) then
            status_tmp_ = CHFL_MEMORY_ERROR
        else
            status_tmp_ = CHFL_SUCCESS
        end if


        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_atom_mass(this, mass, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        real(kind=c_double) :: mass
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_atom_mass(this%ptr, mass)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_atom_set_mass(this, mass, status)
        implicit none
        class(chfl_atom) :: this
        real(kind=c_double), value :: mass
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_atom_set_mass(this%ptr, mass)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_atom_charge(this, charge, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        real(kind=c_double) :: charge
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_atom_charge(this%ptr, charge)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_atom_set_charge(this, charge, status)
        implicit none
        class(chfl_atom) :: this
        real(kind=c_double), value :: charge
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_atom_set_charge(this%ptr, charge)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_atom_type(this, type, buffsize, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        character(len=*) :: type
        integer(kind=c_int64_t), value :: buffsize
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_atom_type(this%ptr, type, buffsize)
        type = rm_null_in_str(type)
        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_atom_set_type(this, type, status)
        implicit none
        class(chfl_atom) :: this
        character(len=*), intent(in) :: type
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_atom_set_type(this%ptr, f_to_c_str(type))

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_atom_name(this, name, buffsize, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        character(len=*) :: name
        integer(kind=c_int64_t), value :: buffsize
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_atom_name(this%ptr, name, buffsize)
        name = rm_null_in_str(name)
        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_atom_set_name(this, name, status)
        implicit none
        class(chfl_atom) :: this
        character(len=*), intent(in) :: name
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_atom_set_name(this%ptr, f_to_c_str(name))

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_atom_full_name(this, name, buffsize, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        character(len=*) :: name
        integer(kind=c_int64_t), value :: buffsize
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_atom_full_name(this%ptr, name, buffsize)
        name = rm_null_in_str(name)
        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_atom_vdw_radius(this, radius, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        real(kind=c_double) :: radius
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_atom_vdw_radius(this%ptr, radius)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_atom_covalent_radius(this, radius, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        real(kind=c_double) :: radius
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_atom_covalent_radius(this%ptr, radius)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_atom_atomic_number(this, number, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        integer(kind=c_int64_t) :: number
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_atom_atomic_number(this%ptr, number)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_atom_properties_count(this, count, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        integer(kind=c_int64_t) :: count
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_atom_properties_count(this%ptr, count)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    ! subroutine chfl_atom_list_properties(this, names, count, status)
    !     implicit none
    !     class(chfl_atom), intent(in) :: this
    !     character, intent(in), dimension(:, :), target :: names
    !     integer(kind=c_int64_t), value :: count
    !     integer(chfl_status), optional :: status
    !     integer(chfl_status) :: status_tmp_
    !
    !     status_tmp_ = c_chfl_atom_list_properties(this%ptr, c_loc(names), count)
    !
    !     if (present(status)) then
    !         status = status_tmp_
    !     end if
    ! end subroutine

    ! subroutine chfl_atom_set_property(this, name, property, status)
    !     implicit none
    !     class(chfl_atom) :: this
    !     character(len=*), intent(in) :: name
    !     class(chfl_property), intent(in) :: property
    !     integer(chfl_status), optional :: status
    !     integer(chfl_status) :: status_tmp_
    !
    !     status_tmp_ = c_chfl_atom_set_property(this%ptr, f_to_c_str(name), property%ptr)
    !
    !     if (present(status)) then
    !         status = status_tmp_
    !     end if
    ! end subroutine

    ! subroutine chfl_atom_get_property_init_(this, atom, name, status)
    !     implicit none
    !     class(chfl_property) :: this
    !     class(chfl_atom), intent(in) :: atom
    !     character(len=*), intent(in) :: name
    !     integer(chfl_status), optional :: status
    !     integer(chfl_status) :: status_tmp_
    !
    !     this%ptr = c_chfl_atom_get_property(atom%ptr, f_to_c_str(name))
    !
    !     if (.not. c_associated(this%ptr)) then
    !         status_tmp_ = CHFL_MEMORY_ERROR
    !     else
    !         status_tmp_ = CHFL_SUCCESS
    !     end if
    !
    !
    !     if (present(status)) then
    !         status = status_tmp_
    !     end if
    ! end subroutine

    subroutine chfl_atom_free(this, status)
        implicit none
        class(chfl_atom), intent(in) :: this
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_atom_free(this%ptr)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

end module
