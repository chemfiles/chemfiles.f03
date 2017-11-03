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


subroutine chfl_topology_init_(this, status)
    implicit none
    class(chfl_topology) :: this
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_topology()

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_from_frame_init_(this, frame, status)
    implicit none
    class(chfl_topology) :: this
    class(chfl_frame), intent(in) :: frame
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_topology_from_frame(frame%ptr)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_copy_init_(this, topology, status)
    implicit none
    class(chfl_topology) :: this
    class(chfl_topology), intent(in) :: topology
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_topology_copy(topology%ptr)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_atoms_count(this, size, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_int64_t) :: size
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_atoms_count(this%ptr, size)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_resize(this, n, status)
    implicit none
    class(chfl_topology) :: this
    integer(kind=c_int64_t), value :: n
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_resize(this%ptr, n)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_add_atom(this, atom, status)
    implicit none
    class(chfl_topology) :: this
    class(chfl_atom), intent(in) :: atom
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_add_atom(this%ptr, atom%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_remove(this, i, status)
    implicit none
    class(chfl_topology) :: this
    integer(kind=c_int64_t), value :: i
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_remove(this%ptr, i)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_bonds_count(this, nbonds, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_int64_t) :: nbonds
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_bonds_count(this%ptr, nbonds)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_angles_count(this, nangles, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_int64_t) :: nangles
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_angles_count(this%ptr, nangles)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_dihedrals_count(this, ndihedrals, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_int64_t) :: ndihedrals
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_dihedrals_count(this%ptr, ndihedrals)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_impropers_count(this, nimpropers, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_int64_t) :: nimpropers
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_impropers_count(this%ptr, nimpropers)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_bonds(this, data, nbonds, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_int64_t), dimension(:, :), target :: data
    integer(kind=c_int64_t), value :: nbonds
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_bonds(this%ptr, c_loc(data), nbonds)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_angles(this, data, nangles, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_int64_t), dimension(:, :), target :: data
    integer(kind=c_int64_t), value :: nangles
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_angles(this%ptr, c_loc(data), nangles)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_dihedrals(this, data, ndihedrals, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_int64_t), dimension(:, :), target :: data
    integer(kind=c_int64_t), value :: ndihedrals
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_dihedrals(this%ptr, c_loc(data), ndihedrals)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_impropers(this, data, nimpropers, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_int64_t), dimension(:, :), target :: data
    integer(kind=c_int64_t), value :: nimpropers
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_impropers(this%ptr, c_loc(data), nimpropers)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_add_bond(this, i, j, status)
    implicit none
    class(chfl_topology) :: this
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_add_bond(this%ptr, i, j)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_remove_bond(this, i, j, status)
    implicit none
    class(chfl_topology) :: this
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_remove_bond(this%ptr, i, j)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_residues_count(this, nresidues, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_int64_t) :: nresidues
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_residues_count(this%ptr, nresidues)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_add_residue(this, residue, status)
    implicit none
    class(chfl_topology) :: this
    class(chfl_residue), intent(in) :: residue
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_add_residue(this%ptr, residue%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_residues_linked(this, first, second, result, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    class(chfl_residue), intent(in) :: first
    class(chfl_residue), intent(in) :: second
    logical(kind=c_bool) :: result
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_residues_linked(this%ptr, first%ptr, second%ptr, result)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_free(this, status)
    implicit none
    class(chfl_topology) :: this
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_topology_free(this%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
    this%ptr = c_null_ptr
end subroutine
