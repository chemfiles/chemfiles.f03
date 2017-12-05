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

subroutine chfl_residue_from_topology_init_(this, topology, i, status)
    implicit none
    class(chfl_residue) :: this
    class(chfl_topology), intent(in) :: topology
    integer(kind=c_int64_t), value :: i
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_residue_from_topology(topology%ptr, i)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_residue_for_atom_init_(this, topology, i, status)
    implicit none
    class(chfl_residue) :: this
    class(chfl_topology), intent(in) :: topology
    integer(kind=c_int64_t), value :: i
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_residue_for_atom(topology%ptr, i)

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

subroutine chfl_residue_atoms_count(this, size, status)
    implicit none
    class(chfl_residue), intent(in) :: this
    integer(kind=c_int64_t) :: size
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_residue_atoms_count(this%ptr, size)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_residue_atoms(this, atoms, natoms, status)
    implicit none
    class(chfl_residue), intent(in) :: this
    integer(kind=c_int64_t), dimension(:), target :: atoms
    integer(kind=c_int64_t), value :: natoms
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

subroutine chfl_residue_free(this, status)
    implicit none
    class(chfl_residue) :: this
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_residue_free(this%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
    this%ptr = c_null_ptr
end subroutine
