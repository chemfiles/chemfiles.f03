! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_topology
    use iso_c_binding
    use chemfiles_cdef
    use chemfiles_strings
    use chemfiles_atom
    use chemfiles_residue
    implicit none

    private
    public :: chfl_topology

    type chfl_topology
        type(c_ptr) :: ptr = c_null_ptr
    contains
        procedure :: init => chfl_topology_init_
        procedure :: copy => chfl_topology_copy_init_
        procedure :: atoms_count => chfl_topology_atoms_count
        procedure :: resize => chfl_topology_resize
        procedure :: add_atom => chfl_topology_add_atom
        procedure :: remove => chfl_topology_remove
        procedure :: bonds_count => chfl_topology_bonds_count
        procedure :: angles_count => chfl_topology_angles_count
        procedure :: dihedrals_count => chfl_topology_dihedrals_count
        procedure :: impropers_count => chfl_topology_impropers_count
        procedure :: bonds => chfl_topology_bonds
        procedure :: angles => chfl_topology_angles
        procedure :: dihedrals => chfl_topology_dihedrals
        procedure :: impropers => chfl_topology_impropers
        procedure :: add_bond => chfl_topology_add_bond
        procedure :: remove_bond => chfl_topology_remove_bond
        procedure :: residues_count => chfl_topology_residues_count
        procedure :: add_residue => chfl_topology_add_residue
        procedure :: residues_linked => chfl_topology_residues_linked
        procedure :: bond_with_order => chfl_topology_bond_with_order
        procedure :: bond_orders => chfl_topology_bond_orders
        procedure :: bond_order => chfl_topology_bond_order
        procedure :: free => chfl_topology_free
    end type

contains
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

    subroutine chfl_topology_atoms_count(this, count, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t) :: count
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_topology_atoms_count(this%ptr, count)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_topology_resize(this, natoms, status)
        implicit none
        class(chfl_topology) :: this
        integer(kind=c_int64_t), value :: natoms
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_topology_resize(this%ptr, natoms)

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

        status_tmp_ = c_chfl_topology_add_atom(this%ptr, atom%as_const_ptr())

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

    subroutine chfl_topology_bonds_count(this, count, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t) :: count
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_topology_bonds_count(this%ptr, count)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_topology_angles_count(this, count, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t) :: count
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_topology_angles_count(this%ptr, count)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_topology_dihedrals_count(this, count, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t) :: count
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_topology_dihedrals_count(this%ptr, count)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_topology_impropers_count(this, count, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t) :: count
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_topology_impropers_count(this%ptr, count)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_topology_bonds(this, data, count, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t), value :: count
        integer(kind=c_int64_t), dimension(count, 2), target :: data
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_topology_bonds(this%ptr, c_loc(data), count)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_topology_angles(this, data, count, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t), value :: count
        integer(kind=c_int64_t), dimension(count, 3), target :: data
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_topology_angles(this%ptr, c_loc(data), count)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_topology_dihedrals(this, data, count, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t), value :: count
        integer(kind=c_int64_t), dimension(count, 4), target :: data
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_topology_dihedrals(this%ptr, c_loc(data), count)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_topology_impropers(this, data, count, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t), value :: count
        integer(kind=c_int64_t), dimension(count, 4), target :: data
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_topology_impropers(this%ptr, c_loc(data), count)

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

    subroutine chfl_topology_residues_count(this, count, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t) :: count
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_topology_residues_count(this%ptr, count)

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

    subroutine chfl_topology_bond_with_order(this, i, j, bond_order, status)
        implicit none
        class(chfl_topology) :: this
        integer(kind=c_int64_t), value :: i
        integer(kind=c_int64_t), value :: j
        integer(chfl_bond_order), value :: bond_order
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_topology_bond_with_order(this%ptr, i, j, bond_order)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_topology_bond_orders(this, orders, nbonds, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t), value :: nbonds
        integer(chfl_bond_order), dimension(nbonds), target :: orders
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_topology_bond_orders(this%ptr, c_loc(orders), nbonds)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_topology_bond_order(this, i, j, order, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t), value :: i
        integer(kind=c_int64_t), value :: j
        integer(chfl_bond_order) :: order
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_topology_bond_order(this%ptr, i, j, order)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_topology_free(this, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_topology_free(this%ptr)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    ! subroutine chfl_residue_from_topology_init_(this, topology, i, status)
    !     implicit none
    !     class(chfl_residue) :: this
    !     class(chfl_topology), intent(in) :: topology
    !     integer(kind=c_int64_t), value :: i
    !     integer(chfl_status), optional :: status
    !     integer(chfl_status) :: status_tmp_
    !
    !     this%ptr = c_chfl_residue_from_topology(topology%ptr, i)
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
    !
    ! subroutine chfl_residue_for_atom_init_(this, topology, i, status)
    !     implicit none
    !     class(chfl_residue) :: this
    !     class(chfl_topology), intent(in) :: topology
    !     integer(kind=c_int64_t), value :: i
    !     integer(chfl_status), optional :: status
    !     integer(chfl_status) :: status_tmp_
    !
    !     this%ptr = c_chfl_residue_for_atom(topology%ptr, i)
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
end module
