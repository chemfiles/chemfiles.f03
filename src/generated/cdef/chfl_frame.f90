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

interface
! Function "chfl_frame", at frame.h:20
function c_chfl_frame() bind(C, name="chfl_frame")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_frame

end function

! Function "chfl_frame_copy", at frame.h:30
function c_chfl_frame_copy(frame) bind(C, name="chfl_frame_copy")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_frame_copy
    type(c_ptr), value, intent(in) :: frame
end function

! Function "chfl_frame_atoms_count", at frame.h:38
function c_chfl_frame_atoms_count(frame, size) bind(C, name="chfl_frame_atoms_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_atoms_count
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_int64_t) :: size
end function

! Function "chfl_frame_positions", at frame.h:57
function c_chfl_frame_positions(frame, positions, size) bind(C, name="chfl_frame_positions")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_positions
    type(c_ptr), value :: frame
    type(c_ptr), value :: positions
    integer(kind=c_int64_t) :: size
end function

! Function "chfl_frame_velocities", at frame.h:80
function c_chfl_frame_velocities(frame, velocities, size) bind(C, name="chfl_frame_velocities")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_velocities
    type(c_ptr), value :: frame
    type(c_ptr), value :: velocities
    integer(kind=c_int64_t) :: size
end function

! Function "chfl_frame_add_atom", at frame.h:92
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

! Function "chfl_frame_remove", at frame.h:105
function c_chfl_frame_remove(frame, i) bind(C, name="chfl_frame_remove")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_remove
    type(c_ptr), value :: frame
    integer(kind=c_int64_t), value :: i
end function

! Function "chfl_frame_resize", at frame.h:117
function c_chfl_frame_resize(frame, size) bind(C, name="chfl_frame_resize")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_resize
    type(c_ptr), value :: frame
    integer(kind=c_int64_t), value :: size
end function

! Function "chfl_frame_add_velocities", at frame.h:129
function c_chfl_frame_add_velocities(frame) bind(C, name="chfl_frame_add_velocities")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_add_velocities
    type(c_ptr), value :: frame
end function

! Function "chfl_frame_has_velocities", at frame.h:137
function c_chfl_frame_has_velocities(frame, has_velocities) bind(C, name="chfl_frame_has_velocities")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_has_velocities
    type(c_ptr), value, intent(in) :: frame
    logical(kind=c_bool) :: has_velocities
end function

! Function "chfl_frame_set_cell", at frame.h:146
function c_chfl_frame_set_cell(frame, cell) bind(C, name="chfl_frame_set_cell")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_set_cell
    type(c_ptr), value :: frame
    type(c_ptr), value, intent(in) :: cell
end function

! Function "chfl_frame_set_topology", at frame.h:158
function c_chfl_frame_set_topology(frame, topology) bind(C, name="chfl_frame_set_topology")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_set_topology
    type(c_ptr), value :: frame
    type(c_ptr), value, intent(in) :: topology
end function

! Function "chfl_frame_step", at frame.h:168
function c_chfl_frame_step(frame, step) bind(C, name="chfl_frame_step")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_step
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_int64_t) :: step
end function

! Function "chfl_frame_set_step", at frame.h:177
function c_chfl_frame_set_step(frame, step) bind(C, name="chfl_frame_set_step")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_set_step
    type(c_ptr), value :: frame
    integer(kind=c_int64_t), value :: step
end function

! Function "chfl_frame_guess_topology", at frame.h:189
function c_chfl_frame_guess_topology(frame) bind(C, name="chfl_frame_guess_topology")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_guess_topology
    type(c_ptr), value :: frame
end function

! Function "chfl_frame_distance", at frame.h:198
function c_chfl_frame_distance(frame, i, j, distance) bind(C, name="chfl_frame_distance")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_distance
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
    real(kind=c_double) :: distance
end function

! Function "chfl_frame_angle", at frame.h:209
function c_chfl_frame_angle(frame, i, j, k, angle) bind(C, name="chfl_frame_angle")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_angle
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
    integer(kind=c_int64_t), value :: k
    real(kind=c_double) :: angle
end function

! Function "chfl_frame_dihedral", at frame.h:220
function c_chfl_frame_dihedral(frame, i, j, k, m, dihedral) bind(C, name="chfl_frame_dihedral")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_dihedral
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
    integer(kind=c_int64_t), value :: k
    integer(kind=c_int64_t), value :: m
    real(kind=c_double) :: dihedral
end function

! Function "chfl_frame_out_of_plane", at frame.h:234
function c_chfl_frame_out_of_plane(frame, i, j, k, m, distance) bind(C, name="chfl_frame_out_of_plane")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_out_of_plane
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
    integer(kind=c_int64_t), value :: k
    integer(kind=c_int64_t), value :: m
    real(kind=c_double) :: distance
end function

! Function "chfl_frame_set_property", at frame.h:246
function c_chfl_frame_set_property(frame, name, property) bind(C, name="chfl_frame_set_property")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_set_property
    type(c_ptr), value :: frame
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
    type(c_ptr), value, intent(in) :: property
end function

! Function "chfl_frame_get_property", at frame.h:260
function c_chfl_frame_get_property(frame, name) bind(C, name="chfl_frame_get_property")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_frame_get_property
    type(c_ptr), value, intent(in) :: frame
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
end function

! Function "chfl_frame_add_bond", at frame.h:269
function c_chfl_frame_add_bond(frame, i, j) bind(C, name="chfl_frame_add_bond")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_add_bond
    type(c_ptr), value :: frame
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
end function

! Function "chfl_frame_remove_bond", at frame.h:281
function c_chfl_frame_remove_bond(frame, i, j) bind(C, name="chfl_frame_remove_bond")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_remove_bond
    type(c_ptr), value :: frame
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
end function

! Function "chfl_frame_add_residue", at frame.h:293
function c_chfl_frame_add_residue(frame, residue) bind(C, name="chfl_frame_add_residue")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_add_residue
    type(c_ptr), value :: frame
    type(c_ptr), value, intent(in) :: residue
end function

! Function "chfl_frame_free", at frame.h:301
function c_chfl_frame_free(frame) bind(C, name="chfl_frame_free")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_free
    type(c_ptr), value :: frame
end function

end interface
