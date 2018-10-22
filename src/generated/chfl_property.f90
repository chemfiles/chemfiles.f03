! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
!
! =========================================================================== !
! !!!! AUTO-GENERATED FILE !!!! Do not edit. See bindgen repository for the
! generating code (https://github.com/chemfiles/bindgen).
! This file contains Fortran 2003 ISO C Binding interface to the C API
! =========================================================================== !

interface
! Function "chfl_property_bool", at property.h:32:28
function c_chfl_property_bool(value) bind(C, name="chfl_property_bool")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_property_bool
    logical(kind=c_bool), value :: value
end function

! Function "chfl_property_double", at property.h:42:28
function c_chfl_property_double(value) bind(C, name="chfl_property_double")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_property_double
    real(kind=c_double), value :: value
end function

! Function "chfl_property_string", at property.h:52:28
function c_chfl_property_string(value) bind(C, name="chfl_property_string")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_property_string
    character(len=1, kind=c_char), dimension(*), intent(in) :: value
end function

! Function "chfl_property_vector3d", at property.h:62:28
function c_chfl_property_vector3d(value) bind(C, name="chfl_property_vector3d")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_property_vector3d
    real(kind=c_double), dimension(3), intent(in) :: value
end function

! Function "chfl_property_get_kind", at property.h:69:25
function c_chfl_property_get_kind(property, kind) bind(C, name="chfl_property_get_kind")
    use iso_c_binding
    import chfl_status
    import chfl_property_kind

    implicit none
    integer(kind=chfl_status) :: c_chfl_property_get_kind
    type(c_ptr), value, intent(in) :: property
    integer(chfl_property_kind), intent(inout) :: kind
end function

! Function "chfl_property_get_bool", at property.h:82:25
function c_chfl_property_get_bool(property, value) bind(C, name="chfl_property_get_bool")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_property_get_bool
    type(c_ptr), value, intent(in) :: property
    logical(kind=c_bool), intent(inout) :: value
end function

! Function "chfl_property_get_double", at property.h:95:25
function c_chfl_property_get_double(property, value) bind(C, name="chfl_property_get_double")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_property_get_double
    type(c_ptr), value, intent(in) :: property
    real(kind=c_double), intent(inout) :: value
end function

! Function "chfl_property_get_string", at property.h:110:25
function c_chfl_property_get_string(property, buffer, buffsize) bind(C, name="chfl_property_get_string")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_property_get_string
    type(c_ptr), value, intent(in) :: property
    character(len=1, kind=c_char), dimension(*) :: buffer
    integer(kind=c_int64_t), value :: buffsize
end function

! Function "chfl_property_get_vector3d", at property.h:123:25
function c_chfl_property_get_vector3d(property, value) bind(C, name="chfl_property_get_vector3d")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_property_get_vector3d
    type(c_ptr), value, intent(in) :: property
    real(kind=c_double), dimension(3), intent(inout) :: value
end function

! Function "chfl_property_free", at property.h:131:25
function c_chfl_property_free(property) bind(C, name="chfl_property_free")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_property_free
    type(c_ptr), value, intent(in) :: property
end function

end interface
