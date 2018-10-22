! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_frame
    use iso_c_binding
    use chemfiles_cdef
    use chemfiles_strings
    use chemfiles_atom
    use chemfiles_cell
    use chemfiles_topology
    use chemfiles_property
    implicit none

    type chfl_frame
        type(c_ptr) :: ptr = c_null_ptr
    contains
        procedure :: init => chfl_frame_init_
        procedure :: copy => chfl_frame_copy_init_
        procedure :: atoms_count => chfl_frame_atoms_count
        procedure :: positions => chfl_frame_positions
        procedure :: velocities => chfl_frame_velocities
        procedure :: add_atom => chfl_frame_add_atom
        procedure :: remove => chfl_frame_remove
        procedure :: resize => chfl_frame_resize
        procedure :: add_velocities => chfl_frame_add_velocities
        procedure :: has_velocities => chfl_frame_has_velocities
        procedure :: set_cell => chfl_frame_set_cell
        procedure :: set_topology => chfl_frame_set_topology
        procedure :: step => chfl_frame_step
        procedure :: set_step => chfl_frame_set_step
        procedure :: guess_bonds => chfl_frame_guess_bonds
        procedure :: distance => chfl_frame_distance
        procedure :: angle => chfl_frame_angle
        procedure :: dihedral => chfl_frame_dihedral
        procedure :: out_of_plane => chfl_frame_out_of_plane
        procedure :: add_bond => chfl_frame_add_bond
        procedure :: bond_with_order => chfl_frame_bond_with_order
        procedure :: remove_bond => chfl_frame_remove_bond
        procedure :: add_residue => chfl_frame_add_residue
        procedure :: free => chfl_frame_free
        ! procedure :: properties_count => chfl_frame_properties_count
        ! procedure :: list_properties => chfl_frame_list_properties
        ! procedure :: set_property => chfl_frame_set_property
        ! procedure :: get_property => chfl_frame_get_property_init_
    end type

contains

    subroutine chfl_frame_init_(this, status)
        implicit none
        class(chfl_frame) :: this
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        this%ptr = c_chfl_frame()

        if (.not. c_associated(this%ptr)) then
            status_tmp_ = CHFL_MEMORY_ERROR
        else
            status_tmp_ = CHFL_SUCCESS
        end if


        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_copy_init_(this, frame, status)
        implicit none
        class(chfl_frame) :: this
        class(chfl_frame), intent(in) :: frame
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        this%ptr = c_chfl_frame_copy(frame%ptr)

        if (.not. c_associated(this%ptr)) then
            status_tmp_ = CHFL_MEMORY_ERROR
        else
            status_tmp_ = CHFL_SUCCESS
        end if


        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_atoms_count(this, count, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        integer(kind=c_int64_t) :: count
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_atoms_count(this%ptr, count)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_positions(this, positions, size, status)
        implicit none
        class(chfl_frame) :: this
        real(kind=c_double), dimension(:, :), pointer :: positions
        integer(kind=c_int64_t) :: size
        integer(chfl_status), optional :: status
        type(c_ptr), target :: c_positions_
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_positions(this%ptr, c_loc(c_positions_), size)
        call c_f_pointer(c_positions_, positions, shape=[3, int(size, c_int)])

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_velocities(this, velocities, size, status)
        implicit none
        class(chfl_frame) :: this
        real(kind=c_double), dimension(:, :), pointer :: velocities
        integer(kind=c_int64_t) :: size
        integer(chfl_status), optional :: status
        type(c_ptr), target :: c_velocities_
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_velocities(this%ptr, c_loc(c_velocities_), size)
        call c_f_pointer(c_velocities_, velocities, shape=[3, int(size, c_int)])

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_add_atom(this, atom, position, velocity, status)
        implicit none
        class(chfl_frame) :: this
        class(chfl_atom), intent(in) :: atom
        real(kind=c_double), dimension(3), intent(in) :: position
        real(kind=c_double), dimension(3), intent(in), optional :: velocity
        integer(chfl_status), optional :: status
        real(kind=c_double), dimension(3) :: velocity_tmp_
        integer(chfl_status) :: status_tmp_


        if (present(velocity)) then
            velocity_tmp_ = velocity
        else
            velocity_tmp_ = [0.0, 0.0, 0.0]
        end if
        status_tmp_ = c_chfl_frame_add_atom(this%ptr, atom%unsafe_const_ptr(), position, velocity_tmp_)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_remove(this, i, status)
        implicit none
        class(chfl_frame) :: this
        integer(kind=c_int64_t), value :: i
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_remove(this%ptr, i)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_resize(this, size, status)
        implicit none
        class(chfl_frame) :: this
        integer(kind=c_int64_t), value :: size
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_resize(this%ptr, size)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_add_velocities(this, status)
        implicit none
        class(chfl_frame) :: this
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_add_velocities(this%ptr)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_has_velocities(this, has_velocities, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        logical(kind=c_bool) :: has_velocities
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_has_velocities(this%ptr, has_velocities)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_set_cell(this, cell, status)
        implicit none
        class(chfl_frame) :: this
        class(chfl_cell), intent(in) :: cell
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_set_cell(this%ptr, cell%ptr)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_set_topology(this, topology, status)
        implicit none
        class(chfl_frame) :: this
        class(chfl_topology), intent(in) :: topology
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_set_topology(this%ptr, topology%ptr)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_step(this, step, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        integer(kind=c_int64_t) :: step
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_step(this%ptr, step)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_set_step(this, step, status)
        implicit none
        class(chfl_frame) :: this
        integer(kind=c_int64_t), value :: step
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_set_step(this%ptr, step)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_guess_bonds(this, status)
        implicit none
        class(chfl_frame) :: this
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_guess_bonds(this%ptr)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_distance(this, i, j, distance, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        integer(kind=c_int64_t), value :: i
        integer(kind=c_int64_t), value :: j
        real(kind=c_double) :: distance
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_distance(this%ptr, i, j, distance)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_angle(this, i, j, k, angle, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        integer(kind=c_int64_t), value :: i
        integer(kind=c_int64_t), value :: j
        integer(kind=c_int64_t), value :: k
        real(kind=c_double) :: angle
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_angle(this%ptr, i, j, k, angle)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_dihedral(this, i, j, k, m, dihedral, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        integer(kind=c_int64_t), value :: i
        integer(kind=c_int64_t), value :: j
        integer(kind=c_int64_t), value :: k
        integer(kind=c_int64_t), value :: m
        real(kind=c_double) :: dihedral
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_dihedral(this%ptr, i, j, k, m, dihedral)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_out_of_plane(this, i, j, k, m, distance, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        integer(kind=c_int64_t), value :: i
        integer(kind=c_int64_t), value :: j
        integer(kind=c_int64_t), value :: k
        integer(kind=c_int64_t), value :: m
        real(kind=c_double) :: distance
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_out_of_plane(this%ptr, i, j, k, m, distance)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_properties_count(this, count, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        integer(kind=c_int64_t) :: count
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_properties_count(this%ptr, count)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    ! subroutine chfl_frame_list_properties(this, names, count, status)
    !     implicit none
    !     class(chfl_frame), intent(in) :: this
    !     character, intent(in), dimension(:, :), target :: names
    !     integer(kind=c_int64_t), value :: count
    !     integer(chfl_status), optional :: status
    !     integer(chfl_status) :: status_tmp_
    !
    !     status_tmp_ = c_chfl_frame_list_properties(this%ptr, c_loc(names), count)
    !
    !     if (present(status)) then
    !         status = status_tmp_
    !     end if
    ! end subroutine

    subroutine chfl_frame_set_property(this, name, property, status)
        implicit none
        class(chfl_frame) :: this
        character(len=*), intent(in) :: name
        class(chfl_property), intent(in) :: property
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_set_property(this%ptr, f_to_c_str(name), property%ptr)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_get_property_init_(this, frame, name, status)
        implicit none
        class(chfl_property) :: this
        class(chfl_frame), intent(in) :: frame
        character(len=*), intent(in) :: name
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        this%ptr = c_chfl_frame_get_property(frame%ptr, f_to_c_str(name))

        if (.not. c_associated(this%ptr)) then
            status_tmp_ = CHFL_MEMORY_ERROR
        else
            status_tmp_ = CHFL_SUCCESS
        end if


        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_add_bond(this, i, j, status)
        implicit none
        class(chfl_frame) :: this
        integer(kind=c_int64_t), value :: i
        integer(kind=c_int64_t), value :: j
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_add_bond(this%ptr, i, j)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_bond_with_order(this, i, j, bond_order, status)
        implicit none
        class(chfl_frame) :: this
        integer(kind=c_int64_t), value :: i
        integer(kind=c_int64_t), value :: j
        integer(chfl_bond_order), value :: bond_order
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_bond_with_order(this%ptr, i, j, bond_order)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_remove_bond(this, i, j, status)
        implicit none
        class(chfl_frame) :: this
        integer(kind=c_int64_t), value :: i
        integer(kind=c_int64_t), value :: j
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_remove_bond(this%ptr, i, j)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_add_residue(this, residue, status)
        implicit none
        class(chfl_frame) :: this
        class(chfl_residue), intent(in) :: residue
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_add_residue(this%ptr, residue%ptr)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_frame_free(this, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_frame_free(this%ptr)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    ! subroutine chfl_atom_from_frame_init_(this, frame, index, status)
    !     implicit none
    !     class(chfl_atom) :: this
    !     class(chfl_frame) :: frame
    !     integer(kind=c_int64_t), value :: index
    !     integer(chfl_status), optional :: status
    !     integer(chfl_status) :: status_tmp_
    !
    !     this%ptr = c_chfl_atom_from_frame(frame%ptr, index)
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

    ! subroutine chfl_atom_from_topology_init_(this, topology, index, status)
    !     implicit none
    !     class(chfl_atom) :: this
    !     class(chfl_topology) :: topology
    !     integer(kind=c_int64_t), value :: index
    !     integer(chfl_status), optional :: status
    !     integer(chfl_status) :: status_tmp_
    !
    !     this%ptr = c_chfl_atom_from_topology(topology%ptr, index)
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

    ! subroutine chfl_cell_from_frame_init_(this, frame, status)
    !     implicit none
    !     class(chfl_cell) :: this
    !     class(chfl_frame) :: frame
    !     integer(chfl_status), optional :: status
    !     integer(chfl_status) :: status_tmp_
    !
    !     this%ptr = c_chfl_cell_from_frame(frame%ptr)
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

    ! subroutine chfl_topology_from_frame_init_(this, frame, status)
    !     implicit none
    !     class(chfl_topology) :: this
    !     class(chfl_frame), intent(in) :: frame
    !     integer(chfl_status), optional :: status
    !     integer(chfl_status) :: status_tmp_
    !
    !     this%ptr = c_chfl_topology_from_frame(frame%ptr)
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
