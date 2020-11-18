! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
!
! =========================================================================== !
! !!!! AUTO-GENERATED FILE !!!! Do not edit. See bindgen repository for the
! generating code (https://github.com/chemfiles/bindgen).
! This file contains Fortran 2003 ISO C Binding interface to the C API
! =========================================================================== !

interface
! Function "chfl_version", at types.h:150
function c_chfl_version() bind(C, name="chfl_version")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_version

end function

! Function "chfl_last_error", at misc.h:23
function c_chfl_last_error() bind(C, name="chfl_last_error")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_last_error

end function

! Function "chfl_clear_errors", at misc.h:33
function c_chfl_clear_errors() bind(C, name="chfl_clear_errors")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_clear_errors

end function

! Function "chfl_set_warning_callback", at misc.h:42
function c_chfl_set_warning_callback(callback) bind(C, name="chfl_set_warning_callback")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_set_warning_callback
    type(c_funptr), value :: callback
end function

! Function "chfl_add_configuration", at misc.h:58
function c_chfl_add_configuration(path) bind(C, name="chfl_add_configuration")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_add_configuration
    character(len=1, kind=c_char), dimension(*), intent(in) :: path
end function

! Function "chfl_formats_list", at misc.h:71
function c_chfl_formats_list(metadata, count) bind(C, name="chfl_formats_list")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_formats_list
    type(c_ptr), value :: metadata
    integer(kind=c_int64_t), intent(inout) :: count
end function

end interface
