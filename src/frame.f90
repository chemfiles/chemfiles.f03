! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_frame
    use iso_c_binding
    use chemfiles_cdef
    use chemfiles_strings
    use chemfiles_atom
    use chemfiles_cell
    use chemfiles_residue
    use chemfiles_topology
    use chemfiles_property
    implicit none

    private
    public :: chfl_frame

    type chfl_frame
        private
        type(c_ptr) :: ptr = c_null_ptr
    contains
        procedure :: unsafe_set_ptr
        procedure :: unsafe_ptr
        procedure :: unsafe_const_ptr

        generic, public :: init => create, copy
        procedure, private :: create, copy

        procedure :: atoms_count
        procedure :: positions
        procedure :: velocities
        procedure :: atom
        procedure :: add_atom
        procedure :: remove
        procedure :: resize
        procedure :: add_velocities
        procedure :: has_velocities
        procedure :: cell
        procedure :: set_cell
        procedure :: topology
        procedure :: set_topology
        procedure :: step
        procedure :: set_step
        procedure :: guess_bonds
        procedure :: distance
        procedure :: angle
        procedure :: dihedral
        procedure :: out_of_plane
        procedure :: add_bond
        procedure :: remove_bond
        procedure :: add_residue
        procedure :: free
        ! procedure :: properties_count => chfl_frame_properties_count
        ! procedure :: list_properties => chfl_frame_list_properties
        ! procedure :: set_property => chfl_frame_set_property
        ! procedure :: get_property => chfl_frame_get_property_init_
    end type

contains

    subroutine unsafe_set_ptr(this, ptr, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        type(c_ptr), intent(in) :: ptr
        integer(chfl_status), optional, intent(out) :: status
        integer(chfl_status) :: dummy

        if (c_associated(this%unsafe_ptr())) then
            print*, "Trying to reset an allocated chfl_frame. Call chfl_frame%free first."
            ! free the allocated memory
            dummy = c_chfl_frame_free(ptr)
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
        class(chfl_frame), intent(inout) :: this
        unsafe_ptr = this%ptr
    end function

    type(c_ptr) function unsafe_const_ptr(this)
        implicit none
        class(chfl_frame), intent(in) :: this
        unsafe_const_ptr = this%ptr
    end function

    subroutine create(this, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(c_chfl_frame(), status)
    end subroutine

    subroutine copy(this, frame, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        type(chfl_frame), intent(in) :: frame
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(c_chfl_frame_copy(frame%unsafe_const_ptr()), status)
    end subroutine

    integer(kind=c_int64_t) function atoms_count(this, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_atoms_count(this%unsafe_const_ptr(), atoms_count)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    function positions(this, status)
        implicit none
        real(kind=c_double), dimension(:, :), pointer :: positions
        class(chfl_frame), intent(inout) :: this
        integer(chfl_status), intent(out), optional :: status

        integer(kind=c_int64_t) :: size
        type(c_ptr), target :: positions_tmp
        integer(chfl_status) :: status_tmp

        size = this%atoms_count()
        status_tmp = c_chfl_frame_positions(this%unsafe_ptr(), c_loc(positions_tmp), size)
        call c_f_pointer(positions_tmp, positions, shape=[3, int(size, c_int)])

        if (present(status)) then
            status = status_tmp
        end if
    end function

    function velocities(this, status)
        implicit none
        real(kind=c_double), dimension(:, :), pointer :: velocities
        class(chfl_frame), intent(inout) :: this
        integer(chfl_status), intent(out), optional :: status

        integer(kind=c_int64_t) :: size
        type(c_ptr), target :: velocity_tmp
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_velocities(this%unsafe_ptr(), c_loc(velocity_tmp), size)
        call c_f_pointer(velocity_tmp, velocities, shape=[3, int(size, c_int)])

        if (present(status)) then
            status = status_tmp
        end if
    end function

    type(chfl_atom) function atom(this, i, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        integer(c_int64_t), intent(in) :: i
        integer(chfl_status), intent(out), optional :: status

        call atom%unsafe_set_ptr(c_chfl_atom_from_frame(this%unsafe_ptr(), i), status)
    end function

    subroutine add_atom(this, atom, position, velocity, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        type(chfl_atom), intent(in) :: atom
        real(kind=c_double), dimension(3), intent(in) :: position
        real(kind=c_double), dimension(3), intent(in), optional :: velocity
        integer(chfl_status), intent(out), optional :: status

        real(kind=c_double), dimension(3) :: velocity_tmp
        integer(chfl_status) :: status_tmp


        if (present(velocity)) then
            velocity_tmp = velocity
        else
            velocity_tmp = [0.0, 0.0, 0.0]
        end if
        status_tmp = c_chfl_frame_add_atom(                                   &
            this%unsafe_ptr(), atom%unsafe_const_ptr(), position, velocity_tmp&
        )

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine remove(this, i, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        integer(kind=c_int64_t), intent(in) :: i
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_remove(this%unsafe_ptr(), i)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine resize(this, size, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        integer(kind=c_int64_t), intent(in) :: size
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_resize(this%unsafe_ptr(), size)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine add_velocities(this, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_add_velocities(this%unsafe_ptr())

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    logical function has_velocities(this, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status

        logical(kind=c_bool) :: result
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_has_velocities(this%unsafe_const_ptr(), result)
        has_velocities = logical(result)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    type(chfl_cell) function cell(this, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        integer(chfl_status), intent(out), optional :: status

        call cell%unsafe_set_ptr(c_chfl_cell_from_frame(this%unsafe_ptr()), status)
    end function

    subroutine set_cell(this, cell, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        type(chfl_cell), intent(in) :: cell
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_set_cell(this%unsafe_ptr(), cell%unsafe_const_ptr())

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    type(chfl_topology) function topology(this, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status

        call topology%unsafe_set_const_ptr(                                   &
            c_chfl_topology_from_frame(this%unsafe_const_ptr()), status       &
        )
    end function

    subroutine set_topology(this, topology, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        type(chfl_topology), intent(in) :: topology
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_set_topology(                               &
            this%unsafe_ptr(), topology%unsafe_const_ptr()                    &
        )

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    integer(kind=c_int64_t) function step(this, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_step(this%unsafe_const_ptr(), step)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine set_step(this, step, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        integer(kind=c_int64_t), intent(in) :: step
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_set_step(this%unsafe_ptr(), step)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine guess_bonds(this, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_guess_bonds(this%unsafe_ptr())

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    real(kind=c_double) function distance(this, i, j, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        integer(kind=c_int64_t), intent(in) :: i
        integer(kind=c_int64_t), intent(in) :: j
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_distance(this%unsafe_const_ptr(), i, j, distance)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    real(kind=c_double) function angle(this, i, j, k, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        integer(kind=c_int64_t), intent(in) :: i
        integer(kind=c_int64_t), intent(in) :: j
        integer(kind=c_int64_t), intent(in) :: k
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_angle(this%unsafe_const_ptr(), i, j, k, angle)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    real(kind=c_double) function dihedral(this, i, j, k, m, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        integer(kind=c_int64_t), intent(in) :: i
        integer(kind=c_int64_t), intent(in) :: j
        integer(kind=c_int64_t), intent(in) :: k
        integer(kind=c_int64_t), intent(in) :: m
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_dihedral(this%unsafe_const_ptr(), i, j, k, m, dihedral)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    real(kind=c_double) function out_of_plane(this, i, j, k, m, status)
        implicit none
        class(chfl_frame), intent(in) :: this
        integer(kind=c_int64_t), intent(in) :: i
        integer(kind=c_int64_t), intent(in) :: j
        integer(kind=c_int64_t), intent(in) :: k
        integer(kind=c_int64_t), intent(in) :: m
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_out_of_plane(this%unsafe_const_ptr(), i, j, k, m, out_of_plane)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    ! subroutine properties_count(this, count, status)
    !     implicit none
    !     class(chfl_frame), intent(in) :: this
    !     integer(kind=c_int64_t) :: count
    !     integer(chfl_status), intent(out), optional :: status
    !     integer(chfl_status) :: status_tmp
    !
    !     status_tmp = c_chfl_frame_properties_count(this%unsafe_ptr(), count)
    !
    !     if (present(status)) then
    !         status = status_tmp
    !     end if
    ! end subroutine

    ! subroutine list_properties(this, names, count, status)
    !     implicit none
    !     class(chfl_frame), intent(in) :: this
    !     character, intent(in), dimension(:, :), target :: names
    !     integer(kind=c_int64_t), intent(in) :: count
    !     integer(chfl_status), intent(out), optional :: status
    !     integer(chfl_status) :: status_tmp
    !
    !     status_tmp = c_chfl_frame_list_properties(this%unsafe_ptr(), c_loc(names), count)
    !
    !     if (present(status)) then
    !         status = status_tmp
    !     end if
    ! end subroutine

    ! subroutine set_property(this, name, property, status)
    !     implicit none
    !     class(chfl_frame), intent(inout) :: this
    !     character(len=*), intent(in) :: name
    !     class(chfl_property), intent(in) :: property
    !     integer(chfl_status), intent(out), optional :: status
    !     integer(chfl_status) :: status_tmp
    !
    !     status_tmp = c_chfl_frame_set_property(this%unsafe_ptr(), f_to_c_str(name), property%unsafe_ptr())
    !
    !     if (present(status)) then
    !         status = status_tmp
    !     end if
    ! end subroutine
    !
    ! subroutine get_property_init_(this, frame, name, status)
    !     implicit none
    !     class(chfl_property) :: this
    !     class(chfl_frame), intent(in) :: frame
    !     character(len=*), intent(in) :: name
    !     integer(chfl_status), intent(out), optional :: status
    !     integer(chfl_status) :: status_tmp
    !
    !     this%unsafe_ptr() = c_chfl_frame_get_property(frame%unsafe_ptr(), f_to_c_str(name))
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

    subroutine add_bond(this, i, j, bond_order, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        integer(kind=c_int64_t), intent(in) :: i
        integer(kind=c_int64_t), intent(in) :: j
        integer(chfl_bond_order), intent(in), optional :: bond_order
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        if (present(bond_order)) then
            status_tmp = c_chfl_frame_bond_with_order(this%unsafe_ptr(), i, j, bond_order)
        else
            status_tmp = c_chfl_frame_add_bond(this%unsafe_ptr(), i, j)
        end if

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine remove_bond(this, i, j, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        integer(kind=c_int64_t), intent(in) :: i
        integer(kind=c_int64_t), intent(in) :: j
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_remove_bond(this%unsafe_ptr(), i, j)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine add_residue(this, residue, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        type(chfl_residue), intent(in) :: residue
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_add_residue(this%unsafe_ptr(), residue%unsafe_const_ptr())

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine free(this, status)
        implicit none
        class(chfl_frame), intent(inout) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_frame_free(this%unsafe_ptr())
        this%ptr = c_null_ptr

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    ! subroutine chfl_atom_from_frame_init_(this, frame, index, status)
    !     implicit none
    !     class(chfl_atom) :: this
    !     class(chfl_frame), intent(inout) :: frame
    !     integer(kind=c_int64_t), intent(in) :: index
    !     integer(chfl_status), intent(out), optional :: status
    !     integer(chfl_status) :: status_tmp
    !
    !     this%unsafe_ptr() = c_chfl_atom_from_frame(frame%unsafe_ptr(), index)
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

    ! subroutine chfl_atom_from_topology_init_(this, topology, index, status)
    !     implicit none
    !     class(chfl_atom), intent(inout) :: this
    !     class(chfl_topology), intent(inout) :: topology
    !     integer(kind=c_int64_t), intent(in) :: index
    !     integer(chfl_status), intent(out), optional :: status
    !     integer(chfl_status) :: status_tmp
    !
    !     this%unsafe_ptr() = c_chfl_atom_from_topology(topology%unsafe_ptr(), index)
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
end module
