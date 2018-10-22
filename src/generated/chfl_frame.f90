! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
!
! =========================================================================== !
! !!!! AUTO-GENERATED FILE !!!! Do not edit. See bindgen repository for the
! generating code (https://github.com/chemfiles/bindgen).
! This file contains Fortran 2003 ISO C Binding interface to the C API
! =========================================================================== !

interface
! Function "chfl_frame", at frame.h:20:25
function c_chfl_frame() bind(C, name="chfl_frame")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_frame

end function

! Function "chfl_frame_copy", at frame.h:30:25
function c_chfl_frame_copy(frame) bind(C, name="chfl_frame_copy")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_frame_copy
    type(c_ptr), value, intent(in) :: frame
end function

! Function "chfl_frame_atoms_count", at frame.h:38:25
function c_chfl_frame_atoms_count(frame, count) bind(C, name="chfl_frame_atoms_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_atoms_count
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_int64_t), intent(inout) :: count
end function

! Function "chfl_frame_positions", at frame.h:57:25
function c_chfl_frame_positions(frame, positions, size) bind(C, name="chfl_frame_positions")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_positions
    type(c_ptr), value :: frame
    type(c_ptr), value :: positions
    integer(kind=c_int64_t), intent(inout) :: size
end function

! Function "chfl_frame_velocities", at frame.h:80:25
function c_chfl_frame_velocities(frame, velocities, size) bind(C, name="chfl_frame_velocities")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_velocities
    type(c_ptr), value :: frame
    type(c_ptr), value :: velocities
    integer(kind=c_int64_t), intent(inout) :: size
end function

! Function "chfl_frame_add_atom", at frame.h:92:25
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

! Function "chfl_frame_remove", at frame.h:105:25
function c_chfl_frame_remove(frame, i) bind(C, name="chfl_frame_remove")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_remove
    type(c_ptr), value :: frame
    integer(kind=c_int64_t), value :: i
end function

! Function "chfl_frame_resize", at frame.h:117:25
function c_chfl_frame_resize(frame, size) bind(C, name="chfl_frame_resize")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_resize
    type(c_ptr), value :: frame
    integer(kind=c_int64_t), value :: size
end function

! Function "chfl_frame_add_velocities", at frame.h:129:25
function c_chfl_frame_add_velocities(frame) bind(C, name="chfl_frame_add_velocities")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_add_velocities
    type(c_ptr), value :: frame
end function

! Function "chfl_frame_has_velocities", at frame.h:137:25
function c_chfl_frame_has_velocities(frame, has_velocities) bind(C, name="chfl_frame_has_velocities")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_has_velocities
    type(c_ptr), value, intent(in) :: frame
    logical(kind=c_bool), intent(inout) :: has_velocities
end function

! Function "chfl_frame_set_cell", at frame.h:146:25
function c_chfl_frame_set_cell(frame, cell) bind(C, name="chfl_frame_set_cell")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_set_cell
    type(c_ptr), value :: frame
    type(c_ptr), value, intent(in) :: cell
end function

! Function "chfl_frame_set_topology", at frame.h:158:25
function c_chfl_frame_set_topology(frame, topology) bind(C, name="chfl_frame_set_topology")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_set_topology
    type(c_ptr), value :: frame
    type(c_ptr), value, intent(in) :: topology
end function

! Function "chfl_frame_step", at frame.h:168:25
function c_chfl_frame_step(frame, step) bind(C, name="chfl_frame_step")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_step
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_int64_t), intent(inout) :: step
end function

! Function "chfl_frame_set_step", at frame.h:177:25
function c_chfl_frame_set_step(frame, step) bind(C, name="chfl_frame_set_step")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_set_step
    type(c_ptr), value :: frame
    integer(kind=c_int64_t), value :: step
end function

! Function "chfl_frame_guess_bonds", at frame.h:189:25
function c_chfl_frame_guess_bonds(frame) bind(C, name="chfl_frame_guess_bonds")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_guess_bonds
    type(c_ptr), value :: frame
end function

! Function "chfl_frame_distance", at frame.h:198:25
function c_chfl_frame_distance(frame, i, j, distance) bind(C, name="chfl_frame_distance")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_distance
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
    real(kind=c_double), intent(inout) :: distance
end function

! Function "chfl_frame_angle", at frame.h:209:25
function c_chfl_frame_angle(frame, i, j, k, angle) bind(C, name="chfl_frame_angle")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_angle
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
    integer(kind=c_int64_t), value :: k
    real(kind=c_double), intent(inout) :: angle
end function

! Function "chfl_frame_dihedral", at frame.h:220:25
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
    real(kind=c_double), intent(inout) :: dihedral
end function

! Function "chfl_frame_out_of_plane", at frame.h:234:25
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
    real(kind=c_double), intent(inout) :: distance
end function

! Function "chfl_frame_properties_count", at frame.h:243:25
function c_chfl_frame_properties_count(frame, count) bind(C, name="chfl_frame_properties_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_properties_count
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_int64_t), intent(inout) :: count
end function

! Function "chfl_frame_list_properties", at frame.h:259:25
function c_chfl_frame_list_properties(frame, names, count) bind(C, name="chfl_frame_list_properties")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_list_properties
    type(c_ptr), value, intent(in) :: frame
    type(c_ptr), value :: names
    integer(kind=c_int64_t), value :: count
end function

! Function "chfl_frame_set_property", at frame.h:271:25
function c_chfl_frame_set_property(frame, name, property) bind(C, name="chfl_frame_set_property")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_set_property
    type(c_ptr), value :: frame
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
    type(c_ptr), value, intent(in) :: property
end function

! Function "chfl_frame_get_property", at frame.h:285:28
function c_chfl_frame_get_property(frame, name) bind(C, name="chfl_frame_get_property")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_frame_get_property
    type(c_ptr), value, intent(in) :: frame
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
end function

! Function "chfl_frame_add_bond", at frame.h:294:25
function c_chfl_frame_add_bond(frame, i, j) bind(C, name="chfl_frame_add_bond")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_add_bond
    type(c_ptr), value :: frame
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
end function

! Function "chfl_frame_bond_with_order", at frame.h:304:25
function c_chfl_frame_bond_with_order(frame, i, j, bond_order) bind(C, name="chfl_frame_bond_with_order")
    use iso_c_binding
    import chfl_status
    import chfl_bond_order

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_bond_with_order
    type(c_ptr), value :: frame
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
    integer(chfl_bond_order), value :: bond_order
end function

! Function "chfl_frame_remove_bond", at frame.h:316:25
function c_chfl_frame_remove_bond(frame, i, j) bind(C, name="chfl_frame_remove_bond")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_remove_bond
    type(c_ptr), value :: frame
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
end function

! Function "chfl_frame_add_residue", at frame.h:328:25
function c_chfl_frame_add_residue(frame, residue) bind(C, name="chfl_frame_add_residue")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_add_residue
    type(c_ptr), value :: frame
    type(c_ptr), value, intent(in) :: residue
end function

! Function "chfl_frame_free", at frame.h:336:25
function c_chfl_frame_free(frame) bind(C, name="chfl_frame_free")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_frame_free
    type(c_ptr), value, intent(in) :: frame
end function

end interface
