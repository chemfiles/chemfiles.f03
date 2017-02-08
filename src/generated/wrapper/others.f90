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


function chfl_version() result(string)
    implicit none

    character(len=1024) :: string
    type(c_ptr) :: c_string

    c_string = c_chfl_version()
    string = c_to_f_str(c_string)
end function

function chfl_last_error() result(string)
    implicit none

    character(len=1024) :: string
    type(c_ptr) :: c_string

    c_string = c_chfl_last_error()
    string = c_to_f_str(c_string)
end function

subroutine chfl_clear_errors(status)
    implicit none
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_clear_errors()
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine
