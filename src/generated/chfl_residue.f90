! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
!
! =========================================================================== !
! !!!! AUTO-GENERATED FILE !!!! Do not edit. See bindgen repository for the
! generating code (https://github.com/chemfiles/bindgen).
! This file contains Fortran 2003 ISO C Binding interface to the C API
! =========================================================================== !

interface
! Function "chfl_residue", at residue.h:25
function c_chfl_residue(name) bind(C, name="chfl_residue")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_residue
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
end function

! Function "chfl_residue_with_id", at residue.h:35
function c_chfl_residue_with_id(name, resid) bind(C, name="chfl_residue_with_id")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_residue_with_id
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
    integer(kind=c_int64_t), value :: resid
end function

! Function "chfl_residue_from_topology", at residue.h:63
function c_chfl_residue_from_topology(topology, i) bind(C, name="chfl_residue_from_topology")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_residue_from_topology
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t), value :: i
end function

! Function "chfl_residue_for_atom", at residue.h:90
function c_chfl_residue_for_atom(topology, i) bind(C, name="chfl_residue_for_atom")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_residue_for_atom
    type(c_ptr), value, intent(in) :: topology
    integer(kind=c_int64_t), value :: i
end function

! Function "chfl_residue_copy", at residue.h:102
function c_chfl_residue_copy(residue) bind(C, name="chfl_residue_copy")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_residue_copy
    type(c_ptr), value, intent(in) :: residue
end function

! Function "chfl_residue_atoms_count", at residue.h:109
function c_chfl_residue_atoms_count(residue, count) bind(C, name="chfl_residue_atoms_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_residue_atoms_count
    type(c_ptr), value, intent(in) :: residue
    integer(kind=c_int64_t), intent(inout) :: count
end function

! Function "chfl_residue_atoms", at residue.h:123
function c_chfl_residue_atoms(residue, atoms, count) bind(C, name="chfl_residue_atoms")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_residue_atoms
    type(c_ptr), value, intent(in) :: residue
    type(c_ptr), value :: atoms
    integer(kind=c_int64_t), value :: count
end function

! Function "chfl_residue_id", at residue.h:136
function c_chfl_residue_id(residue, id) bind(C, name="chfl_residue_id")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_residue_id
    type(c_ptr), value, intent(in) :: residue
    integer(kind=c_int64_t), intent(inout) :: id
end function

! Function "chfl_residue_name", at residue.h:148
function c_chfl_residue_name(residue, name, buffsize) bind(C, name="chfl_residue_name")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_residue_name
    type(c_ptr), value, intent(in) :: residue
    character(len=1, kind=c_char), dimension(*) :: name
    integer(kind=c_int64_t), value :: buffsize
end function

! Function "chfl_residue_add_atom", at residue.h:157
function c_chfl_residue_add_atom(residue, i) bind(C, name="chfl_residue_add_atom")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_residue_add_atom
    type(c_ptr), value :: residue
    integer(kind=c_int64_t), value :: i
end function

! Function "chfl_residue_contains", at residue.h:167
function c_chfl_residue_contains(residue, i, result) bind(C, name="chfl_residue_contains")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_residue_contains
    type(c_ptr), value, intent(in) :: residue
    integer(kind=c_int64_t), value :: i
    logical(kind=c_bool), intent(inout) :: result
end function

! Function "chfl_residue_properties_count", at residue.h:176
function c_chfl_residue_properties_count(residue, count) bind(C, name="chfl_residue_properties_count")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_residue_properties_count
    type(c_ptr), value, intent(in) :: residue
    integer(kind=c_int64_t), intent(inout) :: count
end function

! Function "chfl_residue_list_properties", at residue.h:192
function c_chfl_residue_list_properties(residue, names, count) bind(C, name="chfl_residue_list_properties")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_residue_list_properties
    type(c_ptr), value, intent(in) :: residue
    type(c_ptr), value :: names
    integer(kind=c_int64_t), value :: count
end function

! Function "chfl_residue_set_property", at residue.h:204
function c_chfl_residue_set_property(residue, name, property) bind(C, name="chfl_residue_set_property")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_residue_set_property
    type(c_ptr), value :: residue
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
    type(c_ptr), value, intent(in) :: property
end function

! Function "chfl_residue_get_property", at residue.h:218
function c_chfl_residue_get_property(residue, name) bind(C, name="chfl_residue_get_property")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_residue_get_property
    type(c_ptr), value, intent(in) :: residue
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
end function

end interface
