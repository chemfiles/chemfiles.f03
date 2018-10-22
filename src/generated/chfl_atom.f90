! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
!
! =========================================================================== !
! !!!! AUTO-GENERATED FILE !!!! Do not edit. See bindgen repository for the
! generating code (https://github.com/chemfiles/bindgen).
! This file contains Fortran 2003 ISO C Binding interface to the C API
! =========================================================================== !

interface
! Function "chfl_atom", at atom.h:20:24
function c_chfl_atom(name) bind(C, name="chfl_atom")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_atom
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
end function

! Function "chfl_atom_copy", at atom.h:30:24
function c_chfl_atom_copy(atom) bind(C, name="chfl_atom_copy")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_atom_copy
    type(c_ptr), value, intent(in) :: atom
end function

! Function "chfl_atom_from_frame", at atom.h:56:24
function c_chfl_atom_from_frame(frame, index) bind(C, name="chfl_atom_from_frame")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_atom_from_frame
    type(c_ptr), value :: frame
    integer(kind=c_int64_t), value :: index
end function

! Function "chfl_atom_from_topology", at atom.h:81:24
function c_chfl_atom_from_topology(topology, index) bind(C, name="chfl_atom_from_topology")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_atom_from_topology
    type(c_ptr), value :: topology
    integer(kind=c_int64_t), value :: index
end function

! Function "chfl_atom_mass", at atom.h:92:25
function c_chfl_atom_mass(atom, mass) bind(C, name="chfl_atom_mass")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_mass
    type(c_ptr), value, intent(in) :: atom
    real(kind=c_double), intent(inout) :: mass
end function

! Function "chfl_atom_set_mass", at atom.h:101:25
function c_chfl_atom_set_mass(atom, mass) bind(C, name="chfl_atom_set_mass")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_set_mass
    type(c_ptr), value :: atom
    real(kind=c_double), value :: mass
end function

! Function "chfl_atom_charge", at atom.h:110:25
function c_chfl_atom_charge(atom, charge) bind(C, name="chfl_atom_charge")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_charge
    type(c_ptr), value, intent(in) :: atom
    real(kind=c_double), intent(inout) :: charge
end function

! Function "chfl_atom_set_charge", at atom.h:119:25
function c_chfl_atom_set_charge(atom, charge) bind(C, name="chfl_atom_set_charge")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_set_charge
    type(c_ptr), value :: atom
    real(kind=c_double), value :: charge
end function

! Function "chfl_atom_type", at atom.h:129:25
function c_chfl_atom_type(atom, type, buffsize) bind(C, name="chfl_atom_type")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_type
    type(c_ptr), value, intent(in) :: atom
    character(len=1, kind=c_char), dimension(*) :: type
    integer(kind=c_int64_t), value :: buffsize
end function

! Function "chfl_atom_set_type", at atom.h:140:25
function c_chfl_atom_set_type(atom, type) bind(C, name="chfl_atom_set_type")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_set_type
    type(c_ptr), value :: atom
    character(len=1, kind=c_char), dimension(*), intent(in) :: type
end function

! Function "chfl_atom_name", at atom.h:150:25
function c_chfl_atom_name(atom, name, buffsize) bind(C, name="chfl_atom_name")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_name
    type(c_ptr), value, intent(in) :: atom
    character(len=1, kind=c_char), dimension(*) :: name
    integer(kind=c_int64_t), value :: buffsize
end function

! Function "chfl_atom_set_name", at atom.h:161:25
function c_chfl_atom_set_name(atom, name) bind(C, name="chfl_atom_set_name")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_set_name
    type(c_ptr), value :: atom
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
end function

! Function "chfl_atom_full_name", at atom.h:171:25
function c_chfl_atom_full_name(atom, name, buffsize) bind(C, name="chfl_atom_full_name")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_full_name
    type(c_ptr), value, intent(in) :: atom
    character(len=1, kind=c_char), dimension(*) :: name
    integer(kind=c_int64_t), value :: buffsize
end function

! Function "chfl_atom_vdw_radius", at atom.h:183:25
function c_chfl_atom_vdw_radius(atom, radius) bind(C, name="chfl_atom_vdw_radius")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_vdw_radius
    type(c_ptr), value, intent(in) :: atom
    real(kind=c_double), intent(inout) :: radius
end function

! Function "chfl_atom_covalent_radius", at atom.h:193:25
function c_chfl_atom_covalent_radius(atom, radius) bind(C, name="chfl_atom_covalent_radius")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_covalent_radius
    type(c_ptr), value, intent(in) :: atom
    real(kind=c_double), intent(inout) :: radius
end function

! Function "chfl_atom_atomic_number", at atom.h:203:25
function c_chfl_atom_atomic_number(atom, number) bind(C, name="chfl_atom_atomic_number")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_atomic_number
    type(c_ptr), value, intent(in) :: atom
    integer(kind=c_int64_t), intent(inout) :: number
end function

! Function "chfl_atom_properties_count", at atom.h:210:25
function c_chfl_atom_properties_count(atom, count) bind(C, name="chfl_atom_properties_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_properties_count
    type(c_ptr), value, intent(in) :: atom
    integer(kind=c_int64_t), intent(inout) :: count
end function

! Function "chfl_atom_list_properties", at atom.h:226:25
function c_chfl_atom_list_properties(atom, names, count) bind(C, name="chfl_atom_list_properties")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_list_properties
    type(c_ptr), value, intent(in) :: atom
    type(c_ptr), value :: names
    integer(kind=c_int64_t), value :: count
end function

! Function "chfl_atom_set_property", at atom.h:238:25
function c_chfl_atom_set_property(atom, name, property) bind(C, name="chfl_atom_set_property")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_set_property
    type(c_ptr), value :: atom
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
    type(c_ptr), value, intent(in) :: property
end function

! Function "chfl_atom_get_property", at atom.h:252:28
function c_chfl_atom_get_property(atom, name) bind(C, name="chfl_atom_get_property")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_atom_get_property
    type(c_ptr), value, intent(in) :: atom
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
end function

! Function "chfl_atom_free", at atom.h:260:25
function c_chfl_atom_free(atom) bind(C, name="chfl_atom_free")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_atom_free
    type(c_ptr), value, intent(in) :: atom
end function

end interface
