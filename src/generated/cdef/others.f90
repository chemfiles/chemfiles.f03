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
! Function "chfl_version", at types.h:135
function c_chfl_version() bind(C, name="chfl_version")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_version

end function

! Function "chfl_last_error", at errors.h:20
function c_chfl_last_error() bind(C, name="chfl_last_error")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_last_error

end function

! Function "chfl_clear_errors", at errors.h:27
function c_chfl_clear_errors() bind(C, name="chfl_clear_errors")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_clear_errors

end function

! Function "chfl_set_warning_callback", at errors.h:36
function c_chfl_set_warning_callback(callback) bind(C, name="chfl_set_warning_callback")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_set_warning_callback
    type(c_funptr), value :: callback
end function

end interface
