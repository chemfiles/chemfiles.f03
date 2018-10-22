! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_residue
    use iso_c_binding
    use chemfiles_cdef
    use chemfiles_strings
    implicit none

    type chfl_residue
        type(c_ptr) :: ptr = c_null_ptr
    contains
        procedure :: init => chfl_residue_init_
        procedure :: with_id => chfl_residue_with_id_init_
        procedure :: copy => chfl_residue_copy_init_
        procedure :: atoms_count => chfl_residue_atoms_count
        procedure :: atoms => chfl_residue_atoms
        procedure :: id => chfl_residue_id
        procedure :: name => chfl_residue_name
        procedure :: add_atom => chfl_residue_add_atom
        procedure :: contains => chfl_residue_contains
        procedure :: free => chfl_residue_free
        ! procedure :: properties_count => chfl_residue_properties_count
        ! procedure :: list_properties => chfl_residue_list_properties
        ! procedure :: set_property => chfl_residue_set_property
        ! procedure :: get_property => chfl_residue_get_property_init_
    end type

contains
    subroutine chfl_residue_init_(this, name, status)
        implicit none
        class(chfl_residue) :: this
        character(len=*), intent(in) :: name
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        this%ptr = c_chfl_residue(f_to_c_str(name))

        if (.not. c_associated(this%ptr)) then
            status_tmp_ = CHFL_MEMORY_ERROR
        else
            status_tmp_ = CHFL_SUCCESS
        end if


        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_residue_with_id_init_(this, name, resid, status)
        implicit none
        class(chfl_residue) :: this
        character(len=*), intent(in) :: name
        integer(kind=c_int64_t), value :: resid
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        this%ptr = c_chfl_residue_with_id(f_to_c_str(name), resid)

        if (.not. c_associated(this%ptr)) then
            status_tmp_ = CHFL_MEMORY_ERROR
        else
            status_tmp_ = CHFL_SUCCESS
        end if


        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_residue_copy_init_(this, residue, status)
        implicit none
        class(chfl_residue) :: this
        class(chfl_residue), intent(in) :: residue
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        this%ptr = c_chfl_residue_copy(residue%ptr)

        if (.not. c_associated(this%ptr)) then
            status_tmp_ = CHFL_MEMORY_ERROR
        else
            status_tmp_ = CHFL_SUCCESS
        end if


        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_residue_atoms_count(this, count, status)
        implicit none
        class(chfl_residue), intent(in) :: this
        integer(kind=c_int64_t) :: count
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_residue_atoms_count(this%ptr, count)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_residue_atoms(this, atoms, natoms, status)
        implicit none
        class(chfl_residue), intent(in) :: this
        integer(kind=c_int64_t), value :: natoms
        integer(kind=c_int64_t), dimension(natoms), target :: atoms
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_residue_atoms(this%ptr, c_loc(atoms), natoms)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_residue_id(this, id, status)
        implicit none
        class(chfl_residue), intent(in) :: this
        integer(kind=c_int64_t) :: id
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_residue_id(this%ptr, id)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_residue_name(this, name, buffsize, status)
        implicit none
        class(chfl_residue), intent(in) :: this
        character(len=*) :: name
        integer(kind=c_int64_t), value :: buffsize
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_residue_name(this%ptr, name, buffsize)
        name = rm_null_in_str(name)
        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_residue_add_atom(this, i, status)
        implicit none
        class(chfl_residue) :: this
        integer(kind=c_int64_t), value :: i
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_residue_add_atom(this%ptr, i)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_residue_contains(this, i, result, status)
        implicit none
        class(chfl_residue), intent(in) :: this
        integer(kind=c_int64_t), value :: i
        logical(kind=c_bool) :: result
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_residue_contains(this%ptr, i, result)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_residue_properties_count(this, count, status)
        implicit none
        class(chfl_residue), intent(in) :: this
        integer(kind=c_int64_t) :: count
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_residue_properties_count(this%ptr, count)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    ! subroutine chfl_residue_list_properties(this, names, count, status)
    !     implicit none
    !     class(chfl_residue), intent(in) :: this
    !     character, intent(in), dimension(:, :), target :: names
    !     integer(kind=c_int64_t), value :: count
    !     integer(chfl_status), optional :: status
    !     integer(chfl_status) :: status_tmp_
    !
    !     status_tmp_ = c_chfl_residue_list_properties(this%ptr, c_loc(names), count)
    !
    !     if (present(status)) then
    !         status = status_tmp_
    !     end if
    ! end subroutine

    ! subroutine chfl_residue_set_property(this, name, property, status)
    !     implicit none
    !     class(chfl_residue) :: this
    !     character(len=*), intent(in) :: name
    !     class(chfl_property), intent(in) :: property
    !     integer(chfl_status), optional :: status
    !     integer(chfl_status) :: status_tmp_
    !
    !     status_tmp_ = c_chfl_residue_set_property(this%ptr, f_to_c_str(name), property%ptr)
    !
    !     if (present(status)) then
    !         status = status_tmp_
    !     end if
    ! end subroutine
    !
    ! subroutine chfl_residue_get_property_init_(this, residue, name, status)
    !     implicit none
    !     class(chfl_property) :: this
    !     class(chfl_residue), intent(in) :: residue
    !     character(len=*), intent(in) :: name
    !     integer(chfl_status), optional :: status
    !     integer(chfl_status) :: status_tmp_
    !
    !     this%ptr = c_chfl_residue_get_property(residue%ptr, f_to_c_str(name))
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

    subroutine chfl_residue_free(this, status)
        implicit none
        class(chfl_residue), intent(in) :: this
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_residue_free(this%ptr)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine
end module
