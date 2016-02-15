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


enum, bind(C)
    enumerator :: CHFL_LOG_ERROR = 0
    enumerator :: CHFL_LOG_WARNING = 1
    enumerator :: CHFL_LOG_INFO = 2
    enumerator :: CHFL_LOG_DEBUG = 3
    ! Enumeration name:
    enumerator :: CHFL_LOG_LEVEL
end enum

enum, bind(C)
    enumerator :: CHFL_CELL_ORTHOROMBIC = 0
    enumerator :: CHFL_CELL_TRICLINIC = 1
    enumerator :: CHFL_CELL_INFINITE = 2
    ! Enumeration name:
    enumerator :: CHFL_CELL_TYPES
end enum

enum, bind(C)
    enumerator :: CHFL_ATOM_ELEMENT = 0
    enumerator :: CHFL_ATOM_COARSE_GRAINED = 1
    enumerator :: CHFL_ATOM_DUMMY = 2
    enumerator :: CHFL_ATOM_UNDEFINED = 3
    ! Enumeration name:
    enumerator :: CHFL_ATOM_TYPES
end enum
