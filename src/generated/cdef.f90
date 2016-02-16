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

interface
! Function "chfl_version", at chemfiles.h:73
function chfl_version_c() bind(C, name="chfl_version")
    use iso_c_binding
    implicit none
    type(c_ptr) :: chfl_version_c

end function

! Function "chfl_strerror", at chemfiles.h:80
function chfl_strerror_c(status) bind(C, name="chfl_strerror")
    use iso_c_binding
    implicit none
    type(c_ptr) :: chfl_strerror_c
    integer(kind=c_int), value :: status
end function

! Function "chfl_last_error", at chemfiles.h:86
function chfl_last_error_c() bind(C, name="chfl_last_error")
    use iso_c_binding
    implicit none
    type(c_ptr) :: chfl_last_error_c

end function

! Function "chfl_loglevel", at chemfiles.h:105
function chfl_loglevel_c(level) bind(C, name="chfl_loglevel")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_loglevel_c
    integer(kind=c_int) :: level
end function

! Function "chfl_set_loglevel", at chemfiles.h:112
function chfl_set_loglevel_c(level) bind(C, name="chfl_set_loglevel")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_set_loglevel_c
    integer(kind=c_int), value :: level
end function

! Function "chfl_logfile", at chemfiles.h:119
function chfl_logfile_c(file) bind(C, name="chfl_logfile")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_logfile_c
    character(len=1, kind=c_char), dimension(*), intent(in) :: file
end function

! Function "chfl_log_stdout", at chemfiles.h:125
function chfl_log_stdout_c() bind(C, name="chfl_log_stdout")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_log_stdout_c

end function

! Function "chfl_log_stderr", at chemfiles.h:131
function chfl_log_stderr_c() bind(C, name="chfl_log_stderr")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_log_stderr_c

end function

! Function "chfl_log_silent", at chemfiles.h:137
function chfl_log_silent_c() bind(C, name="chfl_log_silent")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_log_silent_c

end function

! Function "chfl_log_callback", at chemfiles.h:148
function chfl_log_callback_c(callback) bind(C, name="chfl_log_callback")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_log_callback_c
    type(c_funptr), value :: callback
end function

! Function "chfl_trajectory_open", at chemfiles.h:157
function chfl_trajectory_open_c(filename, mode) bind(C, name="chfl_trajectory_open")
    use iso_c_binding
    implicit none
    type(c_ptr) :: chfl_trajectory_open_c
    character(len=1, kind=c_char), dimension(*), intent(in) :: filename
    character(len=1, kind=c_char), dimension(*), intent(in) :: mode
end function

! Function "chfl_trajectory_with_format", at chemfiles.h:166
function chfl_trajectory_with_format_c(filename, mode, format) bind(C, name="chfl_trajectory_with_format")
    use iso_c_binding
    implicit none
    type(c_ptr) :: chfl_trajectory_with_format_c
    character(len=1, kind=c_char), dimension(*), intent(in) :: filename
    character(len=1, kind=c_char), dimension(*), intent(in) :: mode
    character(len=1, kind=c_char), dimension(*), intent(in) :: format
end function

! Function "chfl_trajectory_read", at chemfiles.h:174
function chfl_trajectory_read_c(file, frame) bind(C, name="chfl_trajectory_read")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_trajectory_read_c
    type(c_ptr), value :: file
    type(c_ptr), value :: frame
end function

! Function "chfl_trajectory_read_step", at chemfiles.h:183
function chfl_trajectory_read_step_c(file, step, frame) bind(C, name="chfl_trajectory_read_step")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_trajectory_read_step_c
    type(c_ptr), value :: file
    integer(kind=c_size_t), value :: step
    type(c_ptr), value :: frame
end function

! Function "chfl_trajectory_write", at chemfiles.h:191
function chfl_trajectory_write_c(file, frame) bind(C, name="chfl_trajectory_write")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_trajectory_write_c
    type(c_ptr), value :: file
    type(c_ptr), value, intent(in) :: frame
end function

! Function "chfl_trajectory_set_topology", at chemfiles.h:201
function chfl_trajectory_set_topology_c(file, topology) bind(C, name="chfl_trajectory_set_topology")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_trajectory_set_topology_c
    type(c_ptr), value :: file
    type(c_ptr), value, intent(in) :: topology
end function

! Function "chfl_trajectory_set_topology_file", at chemfiles.h:210
function chfl_trajectory_set_topology_file_c(file, filename) bind(C, name="chfl_trajectory_set_topology_file")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_trajectory_set_topology_file_c
    type(c_ptr), value :: file
    character(len=1, kind=c_char), dimension(*), intent(in) :: filename
end function

! Function "chfl_trajectory_set_cell", at chemfiles.h:220
function chfl_trajectory_set_cell_c(file, cell) bind(C, name="chfl_trajectory_set_cell")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_trajectory_set_cell_c
    type(c_ptr), value :: file
    type(c_ptr), value, intent(in) :: cell
end function

! Function "chfl_trajectory_nsteps", at chemfiles.h:228
function chfl_trajectory_nsteps_c(file, nsteps) bind(C, name="chfl_trajectory_nsteps")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_trajectory_nsteps_c
    type(c_ptr), value :: file
    integer(kind=c_size_t) :: nsteps
end function

! Function "chfl_trajectory_sync", at chemfiles.h:235
function chfl_trajectory_sync_c(file) bind(C, name="chfl_trajectory_sync")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_trajectory_sync_c
    type(c_ptr), value :: file
end function

! Function "chfl_trajectory_close", at chemfiles.h:242
function chfl_trajectory_close_c(file) bind(C, name="chfl_trajectory_close")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_trajectory_close_c
    type(c_ptr), value :: file
end function

! Function "chfl_frame", at chemfiles.h:251
function chfl_frame_c(natoms) bind(C, name="chfl_frame")
    use iso_c_binding
    implicit none
    type(c_ptr) :: chfl_frame_c
    integer(kind=c_size_t), value :: natoms
end function

! Function "chfl_frame_atoms_count", at chemfiles.h:259
function chfl_frame_atoms_count_c(frame, natoms) bind(C, name="chfl_frame_atoms_count")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_frame_atoms_count_c
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_size_t) :: natoms
end function

! Function "chfl_frame_positions", at chemfiles.h:271
function chfl_frame_positions_c(frame, data, size) bind(C, name="chfl_frame_positions")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_frame_positions_c
    type(c_ptr), value :: frame
    type(c_ptr), value :: data
    integer(kind=c_size_t) :: size
end function

! Function "chfl_frame_velocities", at chemfiles.h:287
function chfl_frame_velocities_c(frame, data, size) bind(C, name="chfl_frame_velocities")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_frame_velocities_c
    type(c_ptr), value :: frame
    type(c_ptr), value :: data
    integer(kind=c_size_t) :: size
end function

! Function "chfl_frame_resize", at chemfiles.h:298
function chfl_frame_resize_c(frame, natoms) bind(C, name="chfl_frame_resize")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_frame_resize_c
    type(c_ptr), value :: frame
    integer(kind=c_size_t), value :: natoms
end function

! Function "chfl_frame_add_velocities", at chemfiles.h:307
function chfl_frame_add_velocities_c(frame) bind(C, name="chfl_frame_add_velocities")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_frame_add_velocities_c
    type(c_ptr), value :: frame
end function

! Function "chfl_frame_has_velocities", at chemfiles.h:315
function chfl_frame_has_velocities_c(frame, has_velocities) bind(C, name="chfl_frame_has_velocities")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_frame_has_velocities_c
    type(c_ptr), value, intent(in) :: frame
    logical(kind=c_bool) :: has_velocities
end function

! Function "chfl_frame_set_cell", at chemfiles.h:323
function chfl_frame_set_cell_c(frame, cell) bind(C, name="chfl_frame_set_cell")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_frame_set_cell_c
    type(c_ptr), value :: frame
    type(c_ptr), value, intent(in) :: cell
end function

! Function "chfl_frame_set_topology", at chemfiles.h:331
function chfl_frame_set_topology_c(frame, topology) bind(C, name="chfl_frame_set_topology")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_frame_set_topology_c
    type(c_ptr), value :: frame
    type(c_ptr), value, intent(in) :: topology
end function

! Function "chfl_frame_step", at chemfiles.h:339
function chfl_frame_step_c(frame, step) bind(C, name="chfl_frame_step")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_frame_step_c
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_size_t) :: step
end function

! Function "chfl_frame_set_step", at chemfiles.h:347
function chfl_frame_set_step_c(frame, step) bind(C, name="chfl_frame_set_step")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_frame_set_step_c
    type(c_ptr), value :: frame
    integer(kind=c_size_t), value :: step
end function

! Function "chfl_frame_guess_topology", at chemfiles.h:357
function chfl_frame_guess_topology_c(frame, bonds) bind(C, name="chfl_frame_guess_topology")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_frame_guess_topology_c
    type(c_ptr), value :: frame
    logical(kind=c_bool), value :: bonds
end function

! Function "chfl_frame_selection", at chemfiles.h:380
function chfl_frame_selection_c(frame, selection, matched, natoms) bind(C, name="chfl_frame_selection")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_frame_selection_c
    type(c_ptr), value, intent(in) :: frame
    character(len=1, kind=c_char), dimension(*), intent(in) :: selection
    type(c_ptr), value :: matched
    integer(kind=c_size_t), value :: natoms
end function

! Function "chfl_frame_free", at chemfiles.h:387
function chfl_frame_free_c(frame) bind(C, name="chfl_frame_free")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_frame_free_c
    type(c_ptr), value :: frame
end function

! Function "chfl_cell", at chemfiles.h:397
function chfl_cell_c(a, b, c) bind(C, name="chfl_cell")
    use iso_c_binding
    implicit none
    type(c_ptr) :: chfl_cell_c
    real(kind=c_double), value :: a
    real(kind=c_double), value :: b
    real(kind=c_double), value :: c
end function

! Function "chfl_cell_triclinic", at chemfiles.h:409
function chfl_cell_triclinic_c(a, b, c, alpha, beta, gamma) bind(C, name="chfl_cell_triclinic")
    use iso_c_binding
    implicit none
    type(c_ptr) :: chfl_cell_triclinic_c
    real(kind=c_double), value :: a
    real(kind=c_double), value :: b
    real(kind=c_double), value :: c
    real(kind=c_double), value :: alpha
    real(kind=c_double), value :: beta
    real(kind=c_double), value :: gamma
end function

! Function "chfl_cell_from_frame", at chemfiles.h:416
function chfl_cell_from_frame_c(frame) bind(C, name="chfl_cell_from_frame")
    use iso_c_binding
    implicit none
    type(c_ptr) :: chfl_cell_from_frame_c
    type(c_ptr), value :: frame
end function

! Function "chfl_cell_volume", at chemfiles.h:424
function chfl_cell_volume_c(cell, V) bind(C, name="chfl_cell_volume")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_cell_volume_c
    type(c_ptr), value, intent(in) :: cell
    real(kind=c_double) :: V
end function

! Function "chfl_cell_lengths", at chemfiles.h:434
function chfl_cell_lengths_c(cell, a, b, c) bind(C, name="chfl_cell_lengths")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_cell_lengths_c
    type(c_ptr), value, intent(in) :: cell
    real(kind=c_double) :: a
    real(kind=c_double) :: b
    real(kind=c_double) :: c
end function

! Function "chfl_cell_set_lengths", at chemfiles.h:444
function chfl_cell_set_lengths_c(cell, a, b, c) bind(C, name="chfl_cell_set_lengths")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_cell_set_lengths_c
    type(c_ptr), value :: cell
    real(kind=c_double), value :: a
    real(kind=c_double), value :: b
    real(kind=c_double), value :: c
end function

! Function "chfl_cell_angles", at chemfiles.h:454
function chfl_cell_angles_c(cell, alpha, beta, gamma) bind(C, name="chfl_cell_angles")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_cell_angles_c
    type(c_ptr), value, intent(in) :: cell
    real(kind=c_double) :: alpha
    real(kind=c_double) :: beta
    real(kind=c_double) :: gamma
end function

! Function "chfl_cell_set_angles", at chemfiles.h:464
function chfl_cell_set_angles_c(cell, alpha, beta, gamma) bind(C, name="chfl_cell_set_angles")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_cell_set_angles_c
    type(c_ptr), value :: cell
    real(kind=c_double), value :: alpha
    real(kind=c_double), value :: beta
    real(kind=c_double), value :: gamma
end function

! Function "chfl_cell_matrix", at chemfiles.h:472
function chfl_cell_matrix_c(cell, matrix) bind(C, name="chfl_cell_matrix")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_cell_matrix_c
    type(c_ptr), value, intent(in) :: cell
    type(c_ptr), value :: matrix
end function

! Function "chfl_cell_type", at chemfiles.h:490
function chfl_cell_type_c(cell, type) bind(C, name="chfl_cell_type")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_cell_type_c
    type(c_ptr), value, intent(in) :: cell
    integer(kind=c_int) :: type
end function

! Function "chfl_cell_set_type", at chemfiles.h:498
function chfl_cell_set_type_c(cell, type) bind(C, name="chfl_cell_set_type")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_cell_set_type_c
    type(c_ptr), value :: cell
    integer(kind=c_int), value :: type
end function

! Function "chfl_cell_free", at chemfiles.h:505
function chfl_cell_free_c(cell) bind(C, name="chfl_cell_free")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_cell_free_c
    type(c_ptr), value :: cell
end function

! Function "chfl_topology", at chemfiles.h:513
function chfl_topology_c() bind(C, name="chfl_topology")
    use iso_c_binding
    implicit none
    type(c_ptr) :: chfl_topology_c

end function

! Function "chfl_topology_from_frame", at chemfiles.h:520
function chfl_topology_from_frame_c(frame) bind(C, name="chfl_topology_from_frame")
    use iso_c_binding
    implicit none
    type(c_ptr) :: chfl_topology_from_frame_c
    type(c_ptr), value :: frame
end function

! Function "chfl_topology_atoms_count", at chemfiles.h:528
function chfl_topology_atoms_count_c(topology, natoms) bind(C, name="chfl_topology_atoms_count")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_topology_atoms_count_c
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_size_t) :: natoms
end function

! Function "chfl_topology_append", at chemfiles.h:536
function chfl_topology_append_c(topology, atom) bind(C, name="chfl_topology_append")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_topology_append_c
    type(c_ptr), value :: topology
    type(c_ptr), value, intent(in) :: atom
end function

! Function "chfl_topology_remove", at chemfiles.h:544
function chfl_topology_remove_c(topology, i) bind(C, name="chfl_topology_remove")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_topology_remove_c
    type(c_ptr), value :: topology
    integer(kind=c_size_t), value :: i
end function

! Function "chfl_topology_isbond", at chemfiles.h:554
function chfl_topology_isbond_c(topology, i, j, result) bind(C, name="chfl_topology_isbond")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_topology_isbond_c
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_size_t), value :: i
    integer(kind=c_size_t), value :: j
    logical(kind=c_bool) :: result
end function

! Function "chfl_topology_isangle", at chemfiles.h:565
function chfl_topology_isangle_c(topology, i, j, k, result) bind(C, name="chfl_topology_isangle")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_topology_isangle_c
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_size_t), value :: i
    integer(kind=c_size_t), value :: j
    integer(kind=c_size_t), value :: k
    logical(kind=c_bool) :: result
end function

! Function "chfl_topology_isdihedral", at chemfiles.h:577
function chfl_topology_isdihedral_c(topology, i, j, k, m, result) bind(C, name="chfl_topology_isdihedral")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_topology_isdihedral_c
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_size_t), value :: i
    integer(kind=c_size_t), value :: j
    integer(kind=c_size_t), value :: k
    integer(kind=c_size_t), value :: m
    logical(kind=c_bool) :: result
end function

! Function "chfl_topology_bonds_count", at chemfiles.h:585
function chfl_topology_bonds_count_c(topology, nbonds) bind(C, name="chfl_topology_bonds_count")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_topology_bonds_count_c
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_size_t) :: nbonds
end function

! Function "chfl_topology_angles_count", at chemfiles.h:593
function chfl_topology_angles_count_c(topology, nangles) bind(C, name="chfl_topology_angles_count")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_topology_angles_count_c
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_size_t) :: nangles
end function

! Function "chfl_topology_dihedrals_count", at chemfiles.h:601
function chfl_topology_dihedrals_count_c(topology, ndihedrals) bind(C, name="chfl_topology_dihedrals_count")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_topology_dihedrals_count_c
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_size_t) :: ndihedrals
end function

! Function "chfl_topology_bonds", at chemfiles.h:611
function chfl_topology_bonds_c(topology, data, nbonds) bind(C, name="chfl_topology_bonds")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_topology_bonds_c
    type(c_ptr), value, intent(in) :: topology
    type(c_ptr), value :: data
    integer(kind=c_size_t), value :: nbonds
end function

! Function "chfl_topology_angles", at chemfiles.h:621
function chfl_topology_angles_c(topology, data, nangles) bind(C, name="chfl_topology_angles")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_topology_angles_c
    type(c_ptr), value, intent(in) :: topology
    type(c_ptr), value :: data
    integer(kind=c_size_t), value :: nangles
end function

! Function "chfl_topology_dihedrals", at chemfiles.h:631
function chfl_topology_dihedrals_c(topology, data, ndihedrals) bind(C, name="chfl_topology_dihedrals")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_topology_dihedrals_c
    type(c_ptr), value, intent(in) :: topology
    type(c_ptr), value :: data
    integer(kind=c_size_t), value :: ndihedrals
end function

! Function "chfl_topology_add_bond", at chemfiles.h:640
function chfl_topology_add_bond_c(topology, i, j) bind(C, name="chfl_topology_add_bond")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_topology_add_bond_c
    type(c_ptr), value :: topology
    integer(kind=c_size_t), value :: i
    integer(kind=c_size_t), value :: j
end function

! Function "chfl_topology_remove_bond", at chemfiles.h:649
function chfl_topology_remove_bond_c(topology, i, j) bind(C, name="chfl_topology_remove_bond")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_topology_remove_bond_c
    type(c_ptr), value :: topology
    integer(kind=c_size_t), value :: i
    integer(kind=c_size_t), value :: j
end function

! Function "chfl_topology_free", at chemfiles.h:656
function chfl_topology_free_c(topology) bind(C, name="chfl_topology_free")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_topology_free_c
    type(c_ptr), value :: topology
end function

! Function "chfl_atom", at chemfiles.h:665
function chfl_atom_c(name) bind(C, name="chfl_atom")
    use iso_c_binding
    implicit none
    type(c_ptr) :: chfl_atom_c
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
end function

! Function "chfl_atom_from_frame", at chemfiles.h:673
function chfl_atom_from_frame_c(frame, idx) bind(C, name="chfl_atom_from_frame")
    use iso_c_binding
    implicit none
    type(c_ptr) :: chfl_atom_from_frame_c
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_size_t), value :: idx
end function

! Function "chfl_atom_from_topology", at chemfiles.h:681
function chfl_atom_from_topology_c(topology, idx) bind(C, name="chfl_atom_from_topology")
    use iso_c_binding
    implicit none
    type(c_ptr) :: chfl_atom_from_topology_c
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_size_t), value :: idx
end function

! Function "chfl_atom_mass", at chemfiles.h:689
function chfl_atom_mass_c(atom, mass) bind(C, name="chfl_atom_mass")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_atom_mass_c
    type(c_ptr), value, intent(in) :: atom
    real(kind=c_float) :: mass
end function

! Function "chfl_atom_set_mass", at chemfiles.h:697
function chfl_atom_set_mass_c(atom, mass) bind(C, name="chfl_atom_set_mass")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_atom_set_mass_c
    type(c_ptr), value :: atom
    real(kind=c_float), value :: mass
end function

! Function "chfl_atom_charge", at chemfiles.h:705
function chfl_atom_charge_c(atom, charge) bind(C, name="chfl_atom_charge")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_atom_charge_c
    type(c_ptr), value, intent(in) :: atom
    real(kind=c_float) :: charge
end function

! Function "chfl_atom_set_charge", at chemfiles.h:713
function chfl_atom_set_charge_c(atom, charge) bind(C, name="chfl_atom_set_charge")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_atom_set_charge_c
    type(c_ptr), value :: atom
    real(kind=c_float), value :: charge
end function

! Function "chfl_atom_name", at chemfiles.h:722
function chfl_atom_name_c(atom, name, buffsize) bind(C, name="chfl_atom_name")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_atom_name_c
    type(c_ptr), value, intent(in) :: atom
    character(len=1, kind=c_char), dimension(*) :: name
    integer(kind=c_size_t), value :: buffsize
end function

! Function "chfl_atom_set_name", at chemfiles.h:730
function chfl_atom_set_name_c(atom, name) bind(C, name="chfl_atom_set_name")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_atom_set_name_c
    type(c_ptr), value :: atom
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
end function

! Function "chfl_atom_full_name", at chemfiles.h:739
function chfl_atom_full_name_c(atom, name, buffsize) bind(C, name="chfl_atom_full_name")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_atom_full_name_c
    type(c_ptr), value, intent(in) :: atom
    character(len=1, kind=c_char), dimension(*) :: name
    integer(kind=c_size_t), value :: buffsize
end function

! Function "chfl_atom_vdw_radius", at chemfiles.h:747
function chfl_atom_vdw_radius_c(atom, radius) bind(C, name="chfl_atom_vdw_radius")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_atom_vdw_radius_c
    type(c_ptr), value, intent(in) :: atom
    real(kind=c_double) :: radius
end function

! Function "chfl_atom_covalent_radius", at chemfiles.h:755
function chfl_atom_covalent_radius_c(atom, radius) bind(C, name="chfl_atom_covalent_radius")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_atom_covalent_radius_c
    type(c_ptr), value, intent(in) :: atom
    real(kind=c_double) :: radius
end function

! Function "chfl_atom_atomic_number", at chemfiles.h:763
function chfl_atom_atomic_number_c(atom, number) bind(C, name="chfl_atom_atomic_number")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_atom_atomic_number_c
    type(c_ptr), value, intent(in) :: atom
    integer(kind=c_int) :: number
end function

! Function "chfl_atom_type", at chemfiles.h:784
function chfl_atom_type_c(atom, type) bind(C, name="chfl_atom_type")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_atom_type_c
    type(c_ptr), value, intent(in) :: atom
    integer(kind=c_int) :: type
end function

! Function "chfl_atom_set_type", at chemfiles.h:792
function chfl_atom_set_type_c(atom, type) bind(C, name="chfl_atom_set_type")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_atom_set_type_c
    type(c_ptr), value :: atom
    integer(kind=c_int), value :: type
end function

! Function "chfl_atom_free", at chemfiles.h:799
function chfl_atom_free_c(atom) bind(C, name="chfl_atom_free")
    use iso_c_binding
    implicit none
    integer(c_int) :: chfl_atom_free_c
    type(c_ptr), value :: atom
end function

end interface
