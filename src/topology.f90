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
        private
        type(c_ptr) :: ptr = c_null_ptr
        logical :: is_const = .false.
    contains
        procedure :: unsafe_set_const_ptr
        procedure :: unsafe_set_ptr
        procedure :: unsafe_ptr
        procedure :: unsafe_const_ptr

        generic, public :: init => create, copy
        procedure, private :: create, copy

        procedure :: atoms_count
        procedure :: resize
        procedure :: atom
        procedure :: add_atom
        procedure :: remove
        procedure :: bonds_count
        procedure :: angles_count
        procedure :: dihedrals_count
        procedure :: impropers_count
        procedure :: bonds
        procedure :: angles
        procedure :: dihedrals
        procedure :: impropers
        procedure :: add_bond
        procedure :: remove_bond
        procedure :: residues_count
        procedure :: residue
        procedure :: residue_for_atom
        procedure :: add_residue
        procedure :: residues_linked
        procedure :: bond_orders
        procedure :: bond_order
        procedure :: free
    end type

contains

    subroutine unsafe_set_const_ptr(this, ptr, status)
        implicit none
        class(chfl_topology), intent(inout) :: this
        type(c_ptr), intent(in) :: ptr
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(ptr, status)
        this%is_const = .true.
    end subroutine

    subroutine unsafe_set_ptr(this, ptr, status)
        implicit none
        class(chfl_topology), intent(inout) :: this
        type(c_ptr), intent(in) :: ptr
        integer(chfl_status), optional, intent(out) :: status
        integer(chfl_status) :: dummy

        if (c_associated(this%ptr)) then
            write(*, *) "Trying to reset an allocated chfl_topology. Call chfl_topology%free first."
            ! free the allocated memory
            dummy = c_chfl_topology_free(ptr)
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
        class(chfl_topology), intent(inout) :: this

        if (.not. c_associated(this%ptr)) then
            write(*, *) "Trying to access a NULL chfl_topology. Call chfl_topology%init first."
            stop 1
        else if (this%is_const) then
            write(*, *) "Can not write data to a const chfl_topology"
            stop 1
        end if
        unsafe_ptr = this%ptr
    end function

    type(c_ptr) function unsafe_const_ptr(this)
        implicit none
        class(chfl_topology), intent(in) :: this

        if (.not. c_associated(this%ptr)) then
            write(*, *) "Trying to access a NULL chfl_topology. Call chfl_topology%init first."
            stop 1
        end if
        unsafe_const_ptr = this%ptr
    end function

    subroutine create(this, status)
        implicit none
        class(chfl_topology), intent(inout) :: this
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(c_chfl_topology(), status)
    end subroutine

    subroutine copy(this, topology, status)
        implicit none
        class(chfl_topology), intent(inout) :: this
        class(chfl_topology), intent(in) :: topology
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(c_chfl_topology_copy(topology%unsafe_const_ptr()), status)
    end subroutine

    function atoms_count(this, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t) :: atoms_count
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_topology_atoms_count(this%unsafe_const_ptr(), atoms_count)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine resize(this, natoms, status)
        implicit none
        class(chfl_topology), intent(inout) :: this
        integer(kind=c_int64_t), intent(in) :: natoms
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_topology_resize(this%unsafe_ptr(), natoms)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    type(chfl_atom) function atom(this, i, status)
        implicit none
        class(chfl_topology), intent(inout) :: this
        integer(c_int64_t), intent(in) :: i
        integer(chfl_status), intent(out), optional :: status

        call atom%unsafe_set_ptr(c_chfl_atom_from_topology(this%unsafe_ptr(), i), status)
    end function

    subroutine add_atom(this, atom, status)
        implicit none
        class(chfl_topology), intent(inout) :: this
        class(chfl_atom), intent(in) :: atom
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_topology_add_atom(this%unsafe_ptr(), atom%unsafe_const_ptr())

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine remove(this, i, status)
        implicit none
        class(chfl_topology), intent(inout) :: this
        integer(kind=c_int64_t), intent(in) :: i
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_topology_remove(this%unsafe_ptr(), i)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    function bonds_count(this, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t) :: bonds_count
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_topology_bonds_count(this%unsafe_const_ptr(), bonds_count)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    function angles_count(this, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t) :: angles_count
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_topology_angles_count(this%unsafe_const_ptr(), angles_count)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    function dihedrals_count(this, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t) :: dihedrals_count
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_topology_dihedrals_count(this%unsafe_const_ptr(), dihedrals_count)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    function impropers_count(this, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t) :: impropers_count
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_topology_impropers_count(this%unsafe_const_ptr(), impropers_count)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine bonds(this, data, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t), intent(out), dimension(:, :), target :: data
        integer(chfl_status), intent(out), optional :: status

        integer(chfl_status) :: status_tmp
        integer(kind=c_int64_t), dimension(2) :: data_shape

        data_shape = shape(data, c_int64_t)
        if (data_shape(1) /= 2) then
            if (present(status)) then
                status = CHFL_MEMORY_ERROR
            end if
            return
        end if
        status_tmp = c_chfl_topology_bonds(this%unsafe_const_ptr(), c_loc(data), data_shape(2))

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine angles(this, data, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t), intent(out), dimension(:, :), target :: data
        integer(chfl_status), intent(out), optional :: status

        integer(chfl_status) :: status_tmp
        integer(kind=c_int64_t), dimension(2) :: data_shape

        data_shape = shape(data, c_int64_t)
        if (data_shape(1) /= 3) then
            if (present(status)) then
                status = CHFL_MEMORY_ERROR
            end if
            return
        end if

        status_tmp = c_chfl_topology_angles(this%unsafe_const_ptr(), c_loc(data), data_shape(2))

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine dihedrals(this, data, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t), intent(out), dimension(:, :), target :: data
        integer(chfl_status), intent(out), optional :: status

        integer(chfl_status) :: status_tmp
        integer(kind=c_int64_t), dimension(2) :: data_shape

        data_shape = shape(data, c_int64_t)
        if (data_shape(1) /= 4) then
            if (present(status)) then
                status = CHFL_MEMORY_ERROR
            end if
            return
        end if
        status_tmp = c_chfl_topology_dihedrals(this%unsafe_const_ptr(), c_loc(data), data_shape(2))

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine impropers(this, data, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t), intent(out), dimension(:, :), target :: data
        integer(chfl_status), intent(out), optional :: status

        integer(chfl_status) :: status_tmp
        integer(kind=c_int64_t), dimension(2) :: data_shape

        data_shape = shape(data, c_int64_t)
        if (data_shape(1) /= 4) then
            if (present(status)) then
                status = CHFL_MEMORY_ERROR
            end if
            return
        end if

        status_tmp = c_chfl_topology_impropers(this%unsafe_const_ptr(), c_loc(data), data_shape(2))

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine add_bond(this, i, j, bond_order, status)
        implicit none
        class(chfl_topology), intent(inout) :: this
        integer(kind=c_int64_t), intent(in) :: i
        integer(kind=c_int64_t), intent(in) :: j
        integer(chfl_bond_order), intent(in), optional :: bond_order
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        if (present(bond_order)) then
            status_tmp = c_chfl_topology_bond_with_order(this%unsafe_ptr(), i, j, bond_order)
        else
            status_tmp = c_chfl_topology_add_bond(this%unsafe_ptr(), i, j)
        end if

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine remove_bond(this, i, j, status)
        implicit none
        class(chfl_topology), intent(inout) :: this
        integer(kind=c_int64_t), intent(in) :: i
        integer(kind=c_int64_t), intent(in) :: j
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_topology_remove_bond(this%unsafe_ptr(), i, j)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    integer(kind=c_int64_t) function residues_count(this, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_topology_residues_count(this%unsafe_const_ptr(), residues_count)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    type(chfl_residue) function residue(this, i, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(c_int64_t), intent(in) :: i
        integer(chfl_status), intent(out), optional :: status

        call residue%unsafe_set_const_ptr(                                    &
            c_chfl_residue_from_topology(this%unsafe_const_ptr(), i), status  &
        )
    end function

    type(chfl_residue) function residue_for_atom(this, i, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(c_int64_t), intent(in) :: i
        integer(chfl_status), intent(out), optional :: status

        call residue_for_atom%unsafe_set_const_ptr(                           &
            c_chfl_residue_for_atom(this%unsafe_const_ptr(), i), status       &
        )
    end function

    subroutine add_residue(this, residue, status)
        implicit none
        class(chfl_topology), intent(inout) :: this
        class(chfl_residue), intent(in) :: residue
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_topology_add_residue(this%unsafe_ptr(), residue%unsafe_const_ptr())

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    logical function residues_linked(this, first, second, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        class(chfl_residue), intent(in) :: first
        class(chfl_residue), intent(in) :: second
        integer(chfl_status), intent(out), optional :: status

        integer(chfl_status) :: status_tmp
        logical(c_bool) :: result

        status_tmp = c_chfl_topology_residues_linked(                         &
            this%unsafe_const_ptr(),                                              &
            first%unsafe_const_ptr(),                                             &
            second%unsafe_const_ptr(),                                            &
            result                                                            &
        )
        residues_linked = logical(result)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine bond_orders(this, data, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(chfl_bond_order), intent(out), dimension(:), target :: data
        integer(chfl_status), intent(out), optional :: status

        integer(chfl_status) :: status_tmp
        integer(kind=c_int64_t) :: count

        count = size(data, 1, c_int64_t)
        status_tmp = c_chfl_topology_bond_orders(this%unsafe_const_ptr(), c_loc(data), count)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    integer(chfl_bond_order) function bond_order(this, i, j, status)
        implicit none
        class(chfl_topology), intent(in) :: this
        integer(kind=c_int64_t), intent(in) :: i
        integer(kind=c_int64_t), intent(in) :: j
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_topology_bond_order(this%unsafe_const_ptr(), i, j, bond_order)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine free(this, status)
        implicit none
        class(chfl_topology), intent(inout) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_topology_free(this%ptr)
        this%ptr = c_null_ptr
        this%is_const = .false.

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine
end module
