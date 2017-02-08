! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015 Guillaume Fraux
!
! This Source Code Form is subject to the terms of the Mozilla Public
! License, v. 2.0. If a copy of the MPL was not distributed with this
! file, You can obtain one at http://mozilla.org/MPL/2.0/
!
! =========================================================================== !
! !!!! AUTO-GENERATED FILE !!!! Do not edit. See bindgen repository for the
! generating code (https://github.com/chemfiles/bindgen).
! This file contains Fortran 2003 ISO C Binding interface to the C API
!
! This file is not compilable on his own, but should be 'include'd in another
! fortran compilation unit.
! =========================================================================== !


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

subroutine chfl_frame_atoms_count(this, natoms, status)
    implicit none
    class(chfl_frame), intent(in) :: this
    integer(kind=c_int64_t) :: natoms
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_frame_atoms_count(this%ptr, natoms)
    
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
    status_tmp_ = c_chfl_frame_add_atom(this%ptr, atom%ptr, position, velocity_tmp_)
    
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

subroutine chfl_frame_resize(this, natoms, status)
    implicit none
    class(chfl_frame) :: this
    integer(kind=c_int64_t), value :: natoms
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_frame_resize(this%ptr, natoms)
    
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

subroutine chfl_frame_guess_topology(this, status)
    implicit none
    class(chfl_frame) :: this
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_frame_guess_topology(this%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_frame_free(this, status)
    implicit none
    class(chfl_frame) :: this
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_frame_free(this%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
    this%ptr = c_null_ptr
end subroutine
