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


function chfl_version() result(string)
    implicit none

    character(len=1024) :: string
    type(c_ptr) :: c_string

    c_string = chfl_version_c()
    string = c_to_f_str(c_string)
end function

function chfl_strerror(status) result(string)
    implicit none
    integer(kind=c_int), value :: status
    character(len=1024) :: string
    type(c_ptr) :: c_string

    c_string = chfl_strerror_c(status)
    string = c_to_f_str(c_string)
end function

function chfl_last_error() result(string)
    implicit none

    character(len=1024) :: string
    type(c_ptr) :: c_string

    c_string = chfl_last_error_c()
    string = c_to_f_str(c_string)
end function

subroutine chfl_loglevel(level, status)
    implicit none
    integer(CHFL_LOG_LEVEL) :: level
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_loglevel_c(level)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_set_loglevel(level, status)
    implicit none
    integer(CHFL_LOG_LEVEL), value :: level
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_set_loglevel_c(level)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_logfile(file, status)
    implicit none
    character(len=*), intent(in) :: file
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_logfile_c(f_to_c_str(file))
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_log_stdout(status)
    implicit none

    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_log_stdout_c()
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_log_stderr(status)
    implicit none

    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_log_stderr_c()
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_log_silent(status)
    implicit none

    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_log_silent_c()
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_open_init_(this, filename, mode, status)
    implicit none
    class(chfl_trajectory) :: this
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: mode
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    this%ptr = chfl_trajectory_open_c(f_to_c_str(filename), f_to_c_str(mode))

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = -1
    else
        status_tmp_ = 0
    end if

    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_with_format_init_(this, filename, mode, format, status)
    implicit none
    class(chfl_trajectory) :: this
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: mode
    character(len=*), intent(in) :: format
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    this%ptr = chfl_trajectory_with_format_c(f_to_c_str(filename), f_to_c_str(mode), f_to_c_str(format))

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = -1
    else
        status_tmp_ = 0
    end if

    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_read(this, frame, status)
    implicit none
    class(chfl_trajectory) :: this
    class(chfl_frame) :: frame
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_trajectory_read_c(this%ptr, frame%ptr)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_read_step(this, step, frame, status)
    implicit none
    class(chfl_trajectory) :: this
    integer(kind=c_size_t), value :: step
    class(chfl_frame) :: frame
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_trajectory_read_step_c(this%ptr, step, frame%ptr)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_write(this, frame, status)
    implicit none
    class(chfl_trajectory) :: this
    class(chfl_frame), intent(in) :: frame
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_trajectory_write_c(this%ptr, frame%ptr)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_set_topology(this, topology, status)
    implicit none
    class(chfl_trajectory) :: this
    class(chfl_topology), intent(in) :: topology
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_trajectory_set_topology_c(this%ptr, topology%ptr)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_set_topology_file(this, filename, status)
    implicit none
    class(chfl_trajectory) :: this
    character(len=*), intent(in) :: filename
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_trajectory_set_topology_file_c(this%ptr, f_to_c_str(filename))
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_set_cell(this, cell, status)
    implicit none
    class(chfl_trajectory) :: this
    class(chfl_cell), intent(in) :: cell
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_trajectory_set_cell_c(this%ptr, cell%ptr)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_nsteps(this, nsteps, status)
    implicit none
    class(chfl_trajectory) :: this
    integer(kind=c_size_t) :: nsteps
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_trajectory_nsteps_c(this%ptr, nsteps)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_sync(this, status)
    implicit none
    class(chfl_trajectory) :: this
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_trajectory_sync_c(this%ptr)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_close(this, status)
    implicit none
    class(chfl_trajectory) :: this
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_trajectory_close_c(this%ptr)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_frame_init_(this, natoms, status)
    implicit none
    class(chfl_frame) :: this
    integer(kind=c_size_t), value :: natoms
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    this%ptr = chfl_frame_c(natoms)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = -1
    else
        status_tmp_ = 0
    end if

    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_frame_atoms_count(this, natoms, status)
    implicit none
    class(chfl_frame), intent(in) :: this
    integer(kind=c_size_t) :: natoms
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_frame_atoms_count_c(this%ptr, natoms)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_frame_positions(this, data, size, status)
    implicit none
    class(chfl_frame) :: this
    real(kind=c_float), dimension(:, :), pointer :: data
    integer(kind=c_size_t) :: size
    integer(int32), optional :: status
    integer(int32) :: status_tmp_
    type(c_ptr), target :: c_data_

    status_tmp_ = chfl_frame_positions_c(this%ptr, c_loc(c_data_), size)
    call c_f_pointer(c_data_, data, shape=[3, int(size, int32)])
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_frame_velocities(this, data, size, status)
    implicit none
    class(chfl_frame) :: this
    real(kind=c_float), dimension(:, :), pointer :: data
    integer(kind=c_size_t) :: size
    integer(int32), optional :: status
    integer(int32) :: status_tmp_
    type(c_ptr), target :: c_data_

    status_tmp_ = chfl_frame_velocities_c(this%ptr, c_loc(c_data_), size)
    call c_f_pointer(c_data_, data, shape=[3, int(size, int32)])
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_frame_resize(this, natoms, status)
    implicit none
    class(chfl_frame) :: this
    integer(kind=c_size_t), value :: natoms
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_frame_resize_c(this%ptr, natoms)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_frame_add_velocities(this, status)
    implicit none
    class(chfl_frame) :: this
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_frame_add_velocities_c(this%ptr)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_frame_has_velocities(this, has_velocities, status)
    implicit none
    class(chfl_frame), intent(in) :: this
    logical(kind=c_bool) :: has_velocities
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_frame_has_velocities_c(this%ptr, has_velocities)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_frame_set_cell(this, cell, status)
    implicit none
    class(chfl_frame) :: this
    class(chfl_cell), intent(in) :: cell
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_frame_set_cell_c(this%ptr, cell%ptr)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_frame_set_topology(this, topology, status)
    implicit none
    class(chfl_frame) :: this
    class(chfl_topology), intent(in) :: topology
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_frame_set_topology_c(this%ptr, topology%ptr)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_frame_step(this, step, status)
    implicit none
    class(chfl_frame), intent(in) :: this
    integer(kind=c_size_t) :: step
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_frame_step_c(this%ptr, step)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_frame_set_step(this, step, status)
    implicit none
    class(chfl_frame) :: this
    integer(kind=c_size_t), value :: step
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_frame_set_step_c(this%ptr, step)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_frame_guess_topology(this, bonds, status)
    implicit none
    class(chfl_frame) :: this
    logical(kind=c_bool), value :: bonds
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_frame_guess_topology_c(this%ptr, bonds)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_frame_selection(this, selection, matched, natoms, status)
    implicit none
    class(chfl_frame), intent(in) :: this
    character(len=*), intent(in) :: selection
    logical(kind=c_bool), dimension(:), target :: matched
    integer(kind=c_size_t), value :: natoms
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_frame_selection_c(this%ptr, f_to_c_str(selection), c_loc(matched), natoms)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_frame_free(this, status)
    implicit none
    class(chfl_frame) :: this
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_frame_free_c(this%ptr)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_init_(this, a, b, c, status)
    implicit none
    class(chfl_cell) :: this
    real(kind=c_double), value :: a
    real(kind=c_double), value :: b
    real(kind=c_double), value :: c
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    this%ptr = chfl_cell_c(a, b, c)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = -1
    else
        status_tmp_ = 0
    end if

    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_triclinic_init_(this, a, b, c, alpha, beta, gamma, status)
    implicit none
    class(chfl_cell) :: this
    real(kind=c_double), value :: a
    real(kind=c_double), value :: b
    real(kind=c_double), value :: c
    real(kind=c_double), value :: alpha
    real(kind=c_double), value :: beta
    real(kind=c_double), value :: gamma
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    this%ptr = chfl_cell_triclinic_c(a, b, c, alpha, beta, gamma)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = -1
    else
        status_tmp_ = 0
    end if

    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_from_frame_init_(this, frame, status)
    implicit none
    class(chfl_cell) :: this
    class(chfl_frame) :: frame
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    this%ptr = chfl_cell_from_frame_c(frame%ptr)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = -1
    else
        status_tmp_ = 0
    end if

    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_volume(this, V, status)
    implicit none
    class(chfl_cell), intent(in) :: this
    real(kind=c_double) :: V
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_cell_volume_c(this%ptr, V)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_lengths(this, a, b, c, status)
    implicit none
    class(chfl_cell), intent(in) :: this
    real(kind=c_double) :: a
    real(kind=c_double) :: b
    real(kind=c_double) :: c
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_cell_lengths_c(this%ptr, a, b, c)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_set_lengths(this, a, b, c, status)
    implicit none
    class(chfl_cell) :: this
    real(kind=c_double), value :: a
    real(kind=c_double), value :: b
    real(kind=c_double), value :: c
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_cell_set_lengths_c(this%ptr, a, b, c)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_angles(this, alpha, beta, gamma, status)
    implicit none
    class(chfl_cell), intent(in) :: this
    real(kind=c_double) :: alpha
    real(kind=c_double) :: beta
    real(kind=c_double) :: gamma
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_cell_angles_c(this%ptr, alpha, beta, gamma)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_set_angles(this, alpha, beta, gamma, status)
    implicit none
    class(chfl_cell) :: this
    real(kind=c_double), value :: alpha
    real(kind=c_double), value :: beta
    real(kind=c_double), value :: gamma
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_cell_set_angles_c(this%ptr, alpha, beta, gamma)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_matrix(this, matrix, status)
    implicit none
    class(chfl_cell), intent(in) :: this
    real(kind=c_double), dimension(3, 3), target :: matrix
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_cell_matrix_c(this%ptr, c_loc(matrix))
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_type(this, type, status)
    implicit none
    class(chfl_cell), intent(in) :: this
    integer(CHFL_CELL_TYPES) :: type
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_cell_type_c(this%ptr, type)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_set_type(this, type, status)
    implicit none
    class(chfl_cell) :: this
    integer(CHFL_CELL_TYPES), value :: type
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_cell_set_type_c(this%ptr, type)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_free(this, status)
    implicit none
    class(chfl_cell) :: this
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_cell_free_c(this%ptr)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_init_(this, status)
    implicit none
    class(chfl_topology) :: this
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    this%ptr = chfl_topology_c()

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = -1
    else
        status_tmp_ = 0
    end if

    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_from_frame_init_(this, frame, status)
    implicit none
    class(chfl_topology) :: this
    class(chfl_frame) :: frame
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    this%ptr = chfl_topology_from_frame_c(frame%ptr)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = -1
    else
        status_tmp_ = 0
    end if

    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_atoms_count(this, natoms, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_size_t) :: natoms
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_topology_atoms_count_c(this%ptr, natoms)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_append(this, atom, status)
    implicit none
    class(chfl_topology) :: this
    class(chfl_atom), intent(in) :: atom
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_topology_append_c(this%ptr, atom%ptr)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_remove(this, i, status)
    implicit none
    class(chfl_topology) :: this
    integer(kind=c_size_t), value :: i
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_topology_remove_c(this%ptr, i)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_isbond(this, i, j, result, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_size_t), value :: i
    integer(kind=c_size_t), value :: j
    logical(kind=c_bool) :: result
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_topology_isbond_c(this%ptr, i, j, result)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_isangle(this, i, j, k, result, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_size_t), value :: i
    integer(kind=c_size_t), value :: j
    integer(kind=c_size_t), value :: k
    logical(kind=c_bool) :: result
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_topology_isangle_c(this%ptr, i, j, k, result)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_isdihedral(this, i, j, k, m, result, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_size_t), value :: i
    integer(kind=c_size_t), value :: j
    integer(kind=c_size_t), value :: k
    integer(kind=c_size_t), value :: m
    logical(kind=c_bool) :: result
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_topology_isdihedral_c(this%ptr, i, j, k, m, result)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_bonds_count(this, nbonds, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_size_t) :: nbonds
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_topology_bonds_count_c(this%ptr, nbonds)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_angles_count(this, nangles, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_size_t) :: nangles
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_topology_angles_count_c(this%ptr, nangles)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_dihedrals_count(this, ndihedrals, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_size_t) :: ndihedrals
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_topology_dihedrals_count_c(this%ptr, ndihedrals)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_bonds(this, data, nbonds, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_size_t), dimension(:, :), target :: data
    integer(kind=c_size_t), value :: nbonds
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_topology_bonds_c(this%ptr, c_loc(data), nbonds)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_angles(this, data, nangles, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_size_t), dimension(:, :), target :: data
    integer(kind=c_size_t), value :: nangles
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_topology_angles_c(this%ptr, c_loc(data), nangles)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_dihedrals(this, data, ndihedrals, status)
    implicit none
    class(chfl_topology), intent(in) :: this
    integer(kind=c_size_t), dimension(:, :), target :: data
    integer(kind=c_size_t), value :: ndihedrals
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_topology_dihedrals_c(this%ptr, c_loc(data), ndihedrals)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_add_bond(this, i, j, status)
    implicit none
    class(chfl_topology) :: this
    integer(kind=c_size_t), value :: i
    integer(kind=c_size_t), value :: j
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_topology_add_bond_c(this%ptr, i, j)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_remove_bond(this, i, j, status)
    implicit none
    class(chfl_topology) :: this
    integer(kind=c_size_t), value :: i
    integer(kind=c_size_t), value :: j
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_topology_remove_bond_c(this%ptr, i, j)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_topology_free(this, status)
    implicit none
    class(chfl_topology) :: this
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_topology_free_c(this%ptr)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_init_(this, name, status)
    implicit none
    class(chfl_atom) :: this
    character(len=*), intent(in) :: name
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    this%ptr = chfl_atom_c(f_to_c_str(name))

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = -1
    else
        status_tmp_ = 0
    end if

    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_from_frame_init_(this, frame, idx, status)
    implicit none
    class(chfl_atom) :: this
    class(chfl_frame), intent(in) :: frame
    integer(kind=c_size_t), value :: idx
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    this%ptr = chfl_atom_from_frame_c(frame%ptr, idx)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = -1
    else
        status_tmp_ = 0
    end if

    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_from_topology_init_(this, topology, idx, status)
    implicit none
    class(chfl_atom) :: this
    class(chfl_topology), intent(in) :: topology
    integer(kind=c_size_t), value :: idx
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    this%ptr = chfl_atom_from_topology_c(topology%ptr, idx)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = -1
    else
        status_tmp_ = 0
    end if

    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_mass(this, mass, status)
    implicit none
    class(chfl_atom), intent(in) :: this
    real(kind=c_float) :: mass
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_atom_mass_c(this%ptr, mass)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_set_mass(this, mass, status)
    implicit none
    class(chfl_atom) :: this
    real(kind=c_float), value :: mass
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_atom_set_mass_c(this%ptr, mass)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_charge(this, charge, status)
    implicit none
    class(chfl_atom), intent(in) :: this
    real(kind=c_float) :: charge
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_atom_charge_c(this%ptr, charge)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_set_charge(this, charge, status)
    implicit none
    class(chfl_atom) :: this
    real(kind=c_float), value :: charge
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_atom_set_charge_c(this%ptr, charge)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_name(this, name, buffsize, status)
    implicit none
    class(chfl_atom), intent(in) :: this
    character(len=*) :: name
    integer(kind=c_size_t), value :: buffsize
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_atom_name_c(this%ptr, name, buffsize)
    if (present(status)) then
        status = status_tmp_
    end if
    name = rm_c_null_in_str(name)
end subroutine

subroutine chfl_atom_set_name(this, name, status)
    implicit none
    class(chfl_atom) :: this
    character(len=*), intent(in) :: name
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_atom_set_name_c(this%ptr, f_to_c_str(name))
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_full_name(this, name, buffsize, status)
    implicit none
    class(chfl_atom), intent(in) :: this
    character(len=*) :: name
    integer(kind=c_size_t), value :: buffsize
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_atom_full_name_c(this%ptr, name, buffsize)
    if (present(status)) then
        status = status_tmp_
    end if
    name = rm_c_null_in_str(name)
end subroutine

subroutine chfl_atom_vdw_radius(this, radius, status)
    implicit none
    class(chfl_atom), intent(in) :: this
    real(kind=c_double) :: radius
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_atom_vdw_radius_c(this%ptr, radius)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_covalent_radius(this, radius, status)
    implicit none
    class(chfl_atom), intent(in) :: this
    real(kind=c_double) :: radius
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_atom_covalent_radius_c(this%ptr, radius)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_atomic_number(this, number, status)
    implicit none
    class(chfl_atom), intent(in) :: this
    integer(kind=c_int) :: number
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_atom_atomic_number_c(this%ptr, number)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_type(this, type, status)
    implicit none
    class(chfl_atom), intent(in) :: this
    integer(CHFL_ATOM_TYPES) :: type
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_atom_type_c(this%ptr, type)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_set_type(this, type, status)
    implicit none
    class(chfl_atom) :: this
    integer(CHFL_ATOM_TYPES), value :: type
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_atom_set_type_c(this%ptr, type)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_free(this, status)
    implicit none
    class(chfl_atom) :: this
    integer(int32), optional :: status
    integer(int32) :: status_tmp_

    status_tmp_ = chfl_atom_free_c(this%ptr)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine
