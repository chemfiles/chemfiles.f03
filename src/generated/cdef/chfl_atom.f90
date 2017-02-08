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
! Function "chfl_atom", at atom.h:24
function c_chfl_atom(name) bind(C, name="chfl_atom")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_atom
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
end function

! Function "chfl_atom_copy", at atom.h:34
function c_chfl_atom_copy(atom) bind(C, name="chfl_atom_copy")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_atom_copy
    type(c_ptr), value, intent(in) :: atom
end function

! Function "chfl_atom_from_frame", at atom.h:44
function c_chfl_atom_from_frame(frame, i) bind(C, name="chfl_atom_from_frame")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_atom_from_frame
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_int64_t), value :: i
end function

! Function "chfl_atom_from_topology", at atom.h:56
function c_chfl_atom_from_topology(topology, i) bind(C, name="chfl_atom_from_topology")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_atom_from_topology
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t), value :: i
end function

! Function "chfl_atom_mass", at atom.h:67
function c_chfl_atom_mass(atom, mass) bind(C, name="chfl_atom_mass")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_mass
    type(c_ptr), value, intent(in) :: atom
    real(kind=c_double) :: mass
end function

! Function "chfl_atom_set_mass", at atom.h:78
function c_chfl_atom_set_mass(atom, mass) bind(C, name="chfl_atom_set_mass")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_set_mass
    type(c_ptr), value :: atom
    real(kind=c_double), value :: mass
end function

! Function "chfl_atom_charge", at atom.h:89
function c_chfl_atom_charge(atom, charge) bind(C, name="chfl_atom_charge")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_charge
    type(c_ptr), value, intent(in) :: atom
    real(kind=c_double) :: charge
end function

! Function "chfl_atom_set_charge", at atom.h:100
function c_chfl_atom_set_charge(atom, charge) bind(C, name="chfl_atom_set_charge")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_set_charge
    type(c_ptr), value :: atom
    real(kind=c_double), value :: charge
end function

! Function "chfl_atom_type", at atom.h:110
function c_chfl_atom_type(atom, type, buffsize) bind(C, name="chfl_atom_type")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_type
    type(c_ptr), value, intent(in) :: atom
    character(len=1, kind=c_char), dimension(*) :: type
    integer(kind=c_int64_t), value :: buffsize
end function

! Function "chfl_atom_set_type", at atom.h:121
function c_chfl_atom_set_type(atom, type) bind(C, name="chfl_atom_set_type")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_set_type
    type(c_ptr), value :: atom
    character(len=1, kind=c_char), dimension(*), intent(in) :: type
end function

! Function "chfl_atom_name", at atom.h:133
function c_chfl_atom_name(atom, name, buffsize) bind(C, name="chfl_atom_name")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_name
    type(c_ptr), value, intent(in) :: atom
    character(len=1, kind=c_char), dimension(*) :: name
    integer(kind=c_int64_t), value :: buffsize
end function

! Function "chfl_atom_set_name", at atom.h:144
function c_chfl_atom_set_name(atom, name) bind(C, name="chfl_atom_set_name")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_set_name
    type(c_ptr), value :: atom
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
end function

! Function "chfl_atom_full_name", at atom.h:156
function c_chfl_atom_full_name(atom, name, buffsize) bind(C, name="chfl_atom_full_name")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_full_name
    type(c_ptr), value, intent(in) :: atom
    character(len=1, kind=c_char), dimension(*) :: name
    integer(kind=c_int64_t), value :: buffsize
end function

! Function "chfl_atom_vdw_radius", at atom.h:168
function c_chfl_atom_vdw_radius(atom, radius) bind(C, name="chfl_atom_vdw_radius")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_vdw_radius
    type(c_ptr), value, intent(in) :: atom
    real(kind=c_double) :: radius
end function

! Function "chfl_atom_covalent_radius", at atom.h:180
function c_chfl_atom_covalent_radius(atom, radius) bind(C, name="chfl_atom_covalent_radius")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_covalent_radius
    type(c_ptr), value, intent(in) :: atom
    real(kind=c_double) :: radius
end function

! Function "chfl_atom_atomic_number", at atom.h:192
function c_chfl_atom_atomic_number(atom, number) bind(C, name="chfl_atom_atomic_number")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_atomic_number
    type(c_ptr), value, intent(in) :: atom
    integer(kind=c_int64_t) :: number
end function

! Function "chfl_atom_free", at atom.h:200
function c_chfl_atom_free(atom) bind(C, name="chfl_atom_free")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_free
    type(c_ptr), value :: atom
end function

end interface
