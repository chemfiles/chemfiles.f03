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
! Function "chfl_frame", at frame.h:24
function c_chfl_frame() bind(C, name="chfl_frame")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_frame

end function

! Function "chfl_frame_copy", at frame.h:34
function c_chfl_frame_copy(frame) bind(C, name="chfl_frame_copy")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_frame_copy
    type(c_ptr), value, intent(in) :: frame
end function

! Function "chfl_frame_atoms_count", at frame.h:42
function c_chfl_frame_atoms_count(frame, natoms) bind(C, name="chfl_frame_atoms_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_atoms_count
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_int64_t) :: natoms
end function

! Function "chfl_frame_positions", at frame.h:60
function c_chfl_frame_positions(frame, positions, size) bind(C, name="chfl_frame_positions")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_positions
    type(c_ptr), value :: frame
    type(c_ptr), value :: positions
    integer(kind=c_int64_t) :: size
end function

! Function "chfl_frame_velocities", at frame.h:82
function c_chfl_frame_velocities(frame, velocities, size) bind(C, name="chfl_frame_velocities")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_velocities
    type(c_ptr), value :: frame
    type(c_ptr), value :: velocities
    integer(kind=c_int64_t) :: size
end function

! Function "chfl_frame_add_atom", at frame.h:94
function c_chfl_frame_add_atom(frame, atom, position, velocity) bind(C, name="chfl_frame_add_atom")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_add_atom
    type(c_ptr), value :: frame
    type(c_ptr), value, intent(in) :: atom
    real(kind=c_double), dimension(3), intent(in) :: position
    real(kind=c_double), dimension(3), intent(in) :: velocity
end function

! Function "chfl_frame_remove", at frame.h:107
function c_chfl_frame_remove(frame, i) bind(C, name="chfl_frame_remove")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_remove
    type(c_ptr), value :: frame
    integer(kind=c_int64_t), value :: i
end function

! Function "chfl_frame_resize", at frame.h:119
function c_chfl_frame_resize(frame, natoms) bind(C, name="chfl_frame_resize")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_resize
    type(c_ptr), value :: frame
    integer(kind=c_int64_t), value :: natoms
end function

! Function "chfl_frame_add_velocities", at frame.h:131
function c_chfl_frame_add_velocities(frame) bind(C, name="chfl_frame_add_velocities")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_add_velocities
    type(c_ptr), value :: frame
end function

! Function "chfl_frame_has_velocities", at frame.h:139
function c_chfl_frame_has_velocities(frame, has_velocities) bind(C, name="chfl_frame_has_velocities")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_has_velocities
    type(c_ptr), value, intent(in) :: frame
    logical(kind=c_bool) :: has_velocities
end function

! Function "chfl_frame_set_cell", at frame.h:148
function c_chfl_frame_set_cell(frame, cell) bind(C, name="chfl_frame_set_cell")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_set_cell
    type(c_ptr), value :: frame
    type(c_ptr), value, intent(in) :: cell
end function

! Function "chfl_frame_set_topology", at frame.h:160
function c_chfl_frame_set_topology(frame, topology) bind(C, name="chfl_frame_set_topology")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_set_topology
    type(c_ptr), value :: frame
    type(c_ptr), value, intent(in) :: topology
end function

! Function "chfl_frame_step", at frame.h:170
function c_chfl_frame_step(frame, step) bind(C, name="chfl_frame_step")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_step
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_int64_t) :: step
end function

! Function "chfl_frame_set_step", at frame.h:179
function c_chfl_frame_set_step(frame, step) bind(C, name="chfl_frame_set_step")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_set_step
    type(c_ptr), value :: frame
    integer(kind=c_int64_t), value :: step
end function

! Function "chfl_frame_guess_topology", at frame.h:191
function c_chfl_frame_guess_topology(frame) bind(C, name="chfl_frame_guess_topology")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_guess_topology
    type(c_ptr), value :: frame
end function

! Function "chfl_frame_free", at frame.h:197
function c_chfl_frame_free(frame) bind(C, name="chfl_frame_free")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_free
    type(c_ptr), value :: frame
end function

end interface
