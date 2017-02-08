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
! Function "chfl_topology", at topology.h:24
function c_chfl_topology() bind(C, name="chfl_topology")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_topology

end function

! Function "chfl_topology_from_frame", at topology.h:34
function c_chfl_topology_from_frame(frame) bind(C, name="chfl_topology_from_frame")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_topology_from_frame
    type(c_ptr), value, intent(in) :: frame
end function

! Function "chfl_topology_copy", at topology.h:46
function c_chfl_topology_copy(topology) bind(C, name="chfl_topology_copy")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_topology_copy
    type(c_ptr), value, intent(in) :: topology
end function

! Function "chfl_topology_atoms_count", at topology.h:54
function c_chfl_topology_atoms_count(topology, natoms) bind(C, name="chfl_topology_atoms_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_atoms_count
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t) :: natoms
end function

! Function "chfl_topology_resize", at topology.h:66
function c_chfl_topology_resize(topology, natoms) bind(C, name="chfl_topology_resize")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_resize
    type(c_ptr), value :: topology
    integer(kind=c_int64_t), value :: natoms
end function

! Function "chfl_topology_add_atom", at topology.h:75
function c_chfl_topology_add_atom(topology, atom) bind(C, name="chfl_topology_add_atom")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_add_atom
    type(c_ptr), value :: topology
    type(c_ptr), value, intent(in) :: atom
end function

! Function "chfl_topology_remove", at topology.h:86
function c_chfl_topology_remove(topology, i) bind(C, name="chfl_topology_remove")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_remove
    type(c_ptr), value :: topology
    integer(kind=c_int64_t), value :: i
end function

! Function "chfl_topology_isbond", at topology.h:96
function c_chfl_topology_isbond(topology, i, j, result) bind(C, name="chfl_topology_isbond")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_isbond
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
    logical(kind=c_bool) :: result
end function

! Function "chfl_topology_isangle", at topology.h:106
function c_chfl_topology_isangle(topology, i, j, k, result) bind(C, name="chfl_topology_isangle")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_isangle
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
    integer(kind=c_int64_t), value :: k
    logical(kind=c_bool) :: result
end function

! Function "chfl_topology_isdihedral", at topology.h:120
function c_chfl_topology_isdihedral(topology, i, j, k, m, result) bind(C, name="chfl_topology_isdihedral")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_isdihedral
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
    integer(kind=c_int64_t), value :: k
    integer(kind=c_int64_t), value :: m
    logical(kind=c_bool) :: result
end function

! Function "chfl_topology_bonds_count", at topology.h:134
function c_chfl_topology_bonds_count(topology, nbonds) bind(C, name="chfl_topology_bonds_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_bonds_count
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t) :: nbonds
end function

! Function "chfl_topology_angles_count", at topology.h:143
function c_chfl_topology_angles_count(topology, nangles) bind(C, name="chfl_topology_angles_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_angles_count
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t) :: nangles
end function

! Function "chfl_topology_dihedrals_count", at topology.h:152
function c_chfl_topology_dihedrals_count(topology, ndihedrals) bind(C, name="chfl_topology_dihedrals_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_dihedrals_count
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t) :: ndihedrals
end function

! Function "chfl_topology_bonds", at topology.h:165
function c_chfl_topology_bonds(topology, data, nbonds) bind(C, name="chfl_topology_bonds")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_bonds
    type(c_ptr), value, intent(in) :: topology
    type(c_ptr), value :: data
    integer(kind=c_int64_t), value :: nbonds
end function

! Function "chfl_topology_angles", at topology.h:178
function c_chfl_topology_angles(topology, data, nangles) bind(C, name="chfl_topology_angles")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_angles
    type(c_ptr), value, intent(in) :: topology
    type(c_ptr), value :: data
    integer(kind=c_int64_t), value :: nangles
end function

! Function "chfl_topology_dihedrals", at topology.h:191
function c_chfl_topology_dihedrals(topology, data, ndihedrals) bind(C, name="chfl_topology_dihedrals")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_dihedrals
    type(c_ptr), value, intent(in) :: topology
    type(c_ptr), value :: data
    integer(kind=c_int64_t), value :: ndihedrals
end function

! Function "chfl_topology_add_bond", at topology.h:200
function c_chfl_topology_add_bond(topology, i, j) bind(C, name="chfl_topology_add_bond")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_add_bond
    type(c_ptr), value :: topology
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
end function

! Function "chfl_topology_remove_bond", at topology.h:212
function c_chfl_topology_remove_bond(topology, i, j) bind(C, name="chfl_topology_remove_bond")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_remove_bond
    type(c_ptr), value :: topology
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
end function

! Function "chfl_topology_residues_count", at topology.h:222
function c_chfl_topology_residues_count(topology, nresidues) bind(C, name="chfl_topology_residues_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_residues_count
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t) :: nresidues
end function

! Function "chfl_topology_add_residue", at topology.h:234
function c_chfl_topology_add_residue(topology, residue) bind(C, name="chfl_topology_add_residue")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_add_residue
    type(c_ptr), value :: topology
    type(c_ptr), value, intent(in) :: residue
end function

! Function "chfl_topology_residues_linked", at topology.h:245
function c_chfl_topology_residues_linked(topology, first, second, result) bind(C, name="chfl_topology_residues_linked")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_residues_linked
    type(c_ptr), value, intent(in) :: topology
    type(c_ptr), value, intent(in) :: first
    type(c_ptr), value, intent(in) :: second
    logical(kind=c_bool) :: result
end function

! Function "chfl_topology_free", at topology.h:256
function c_chfl_topology_free(topology) bind(C, name="chfl_topology_free")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_free
    type(c_ptr), value :: topology
end function

end interface
