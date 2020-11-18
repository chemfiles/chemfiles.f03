! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
!
! =========================================================================== !
! !!!! AUTO-GENERATED FILE !!!! Do not edit. See bindgen repository for the
! generating code (https://github.com/chemfiles/bindgen).
! This file contains Fortran 2003 ISO C Binding interface to the C API
! =========================================================================== !

interface
! Function "chfl_topology", at topology.h:25
function c_chfl_topology() bind(C, name="chfl_topology")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_topology

end function

! Function "chfl_topology_from_frame", at topology.h:38
function c_chfl_topology_from_frame(frame) bind(C, name="chfl_topology_from_frame")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_topology_from_frame
    type(c_ptr), value, intent(in) :: frame
end function

! Function "chfl_topology_copy", at topology.h:48
function c_chfl_topology_copy(topology) bind(C, name="chfl_topology_copy")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_topology_copy
    type(c_ptr), value, intent(in) :: topology
end function

! Function "chfl_topology_atoms_count", at topology.h:56
function c_chfl_topology_atoms_count(topology, count) bind(C, name="chfl_topology_atoms_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_atoms_count
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t), intent(inout) :: count
end function

! Function "chfl_topology_resize", at topology.h:68
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

! Function "chfl_topology_bonds_count", at topology.h:95
function c_chfl_topology_bonds_count(topology, count) bind(C, name="chfl_topology_bonds_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_bonds_count
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t), intent(inout) :: count
end function

! Function "chfl_topology_angles_count", at topology.h:104
function c_chfl_topology_angles_count(topology, count) bind(C, name="chfl_topology_angles_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_angles_count
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t), intent(inout) :: count
end function

! Function "chfl_topology_dihedrals_count", at topology.h:113
function c_chfl_topology_dihedrals_count(topology, count) bind(C, name="chfl_topology_dihedrals_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_dihedrals_count
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t), intent(inout) :: count
end function

! Function "chfl_topology_impropers_count", at topology.h:122
function c_chfl_topology_impropers_count(topology, count) bind(C, name="chfl_topology_impropers_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_impropers_count
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t), intent(inout) :: count
end function

! Function "chfl_topology_bonds", at topology.h:135
function c_chfl_topology_bonds(topology, data, count) bind(C, name="chfl_topology_bonds")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_bonds
    type(c_ptr), value, intent(in) :: topology
    type(c_ptr), value :: data
    integer(kind=c_int64_t), value :: count
end function

! Function "chfl_topology_angles", at topology.h:148
function c_chfl_topology_angles(topology, data, count) bind(C, name="chfl_topology_angles")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_angles
    type(c_ptr), value, intent(in) :: topology
    type(c_ptr), value :: data
    integer(kind=c_int64_t), value :: count
end function

! Function "chfl_topology_dihedrals", at topology.h:162
function c_chfl_topology_dihedrals(topology, data, count) bind(C, name="chfl_topology_dihedrals")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_dihedrals
    type(c_ptr), value, intent(in) :: topology
    type(c_ptr), value :: data
    integer(kind=c_int64_t), value :: count
end function

! Function "chfl_topology_impropers", at topology.h:176
function c_chfl_topology_impropers(topology, data, count) bind(C, name="chfl_topology_impropers")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_impropers
    type(c_ptr), value, intent(in) :: topology
    type(c_ptr), value :: data
    integer(kind=c_int64_t), value :: count
end function

! Function "chfl_topology_add_bond", at topology.h:185
function c_chfl_topology_add_bond(topology, i, j) bind(C, name="chfl_topology_add_bond")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_add_bond
    type(c_ptr), value :: topology
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
end function

! Function "chfl_topology_remove_bond", at topology.h:197
function c_chfl_topology_remove_bond(topology, i, j) bind(C, name="chfl_topology_remove_bond")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_remove_bond
    type(c_ptr), value :: topology
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
end function

! Function "chfl_topology_clear_bonds", at topology.h:207
function c_chfl_topology_clear_bonds(topology) bind(C, name="chfl_topology_clear_bonds")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_clear_bonds
    type(c_ptr), value :: topology
end function

! Function "chfl_topology_residues_count", at topology.h:215
function c_chfl_topology_residues_count(topology, count) bind(C, name="chfl_topology_residues_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_residues_count
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t), intent(inout) :: count
end function

! Function "chfl_topology_add_residue", at topology.h:227
function c_chfl_topology_add_residue(topology, residue) bind(C, name="chfl_topology_add_residue")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_add_residue
    type(c_ptr), value :: topology
    type(c_ptr), value, intent(in) :: residue
end function

! Function "chfl_topology_residues_linked", at topology.h:238
function c_chfl_topology_residues_linked(topology, first, second, result) bind(C, name="chfl_topology_residues_linked")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_residues_linked
    type(c_ptr), value, intent(in) :: topology
    type(c_ptr), value, intent(in) :: first
    type(c_ptr), value, intent(in) :: second
    logical(kind=c_bool), intent(inout) :: result
end function

! Function "chfl_topology_bond_with_order", at topology.h:251
function c_chfl_topology_bond_with_order(topology, i, j, bond_order) bind(C, name="chfl_topology_bond_with_order")
    use iso_c_binding
    import chfl_status
    import chfl_bond_order

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_bond_with_order
    type(c_ptr), value :: topology
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
    integer(chfl_bond_order), value :: bond_order
end function

! Function "chfl_topology_bond_orders", at topology.h:265
function c_chfl_topology_bond_orders(topology, orders, nbonds) bind(C, name="chfl_topology_bond_orders")
    use iso_c_binding
    import chfl_status
    import chfl_bond_order

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_bond_orders
    type(c_ptr), value, intent(in) :: topology
    type(c_ptr), value :: orders
    integer(kind=c_int64_t), value :: nbonds
end function

! Function "chfl_topology_bond_order", at topology.h:278
function c_chfl_topology_bond_order(topology, i, j, order) bind(C, name="chfl_topology_bond_order")
    use iso_c_binding
    import chfl_status
    import chfl_bond_order

    implicit none
    integer(kind=chfl_status) :: c_chfl_topology_bond_order
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t), value :: i
    integer(kind=c_int64_t), value :: j
    integer(chfl_bond_order), intent(inout) :: order
end function

end interface
