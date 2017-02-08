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
! Function "chfl_selection", at selection.h:24
function c_chfl_selection(selection) bind(C, name="chfl_selection")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_selection
    character(len=1, kind=c_char), dimension(*), intent(in) :: selection
end function

! Function "chfl_selection_copy", at selection.h:37
function c_chfl_selection_copy(selection) bind(C, name="chfl_selection_copy")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_selection_copy
    type(c_ptr), value, intent(in) :: selection
end function

! Function "chfl_selection_size", at selection.h:49
function c_chfl_selection_size(selection, size) bind(C, name="chfl_selection_size")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_selection_size
    type(c_ptr), value, intent(in) :: selection
    integer(kind=c_int64_t) :: size
end function

! Function "chfl_selection_string", at selection.h:62
function c_chfl_selection_string(selection, string, buffsize) bind(C, name="chfl_selection_string")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_selection_string
    type(c_ptr), value, intent(in) :: selection
    character(len=1, kind=c_char), dimension(*) :: string
    integer(kind=c_int64_t), value :: buffsize
end function

! Function "chfl_selection_evaluate", at selection.h:75
function c_chfl_selection_evaluate(selection, frame, nmatches) bind(C, name="chfl_selection_evaluate")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_selection_evaluate
    type(c_ptr), value :: selection
    type(c_ptr), value, intent(in) :: frame
    integer(kind=c_int64_t) :: nmatches
end function

! Function "chfl_selection_matches", at selection.h:101
function c_chfl_selection_matches(selection, matches, nmatches) bind(C, name="chfl_selection_matches")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_selection_matches
    type(c_ptr), value, intent(in) :: selection
    type(c_ptr), value :: matches
    integer(kind=c_int64_t), value :: nmatches
end function

! Function "chfl_selection_free", at selection.h:109
function c_chfl_selection_free(selection) bind(C, name="chfl_selection_free")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_selection_free
    type(c_ptr), value :: selection
end function

end interface
