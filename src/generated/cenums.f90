! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
!
! =========================================================================== !
! !!!! AUTO-GENERATED FILE !!!! Do not edit. See bindgen repository for the
! generating code (https://github.com/chemfiles/bindgen).
! This file contains Fortran 2003 ISO C Binding interface to the C API
! =========================================================================== !


enum, bind(C)
    enumerator :: CHFL_SUCCESS = 0
    enumerator :: CHFL_MEMORY_ERROR = 1
    enumerator :: CHFL_FILE_ERROR = 2
    enumerator :: CHFL_FORMAT_ERROR = 3
    enumerator :: CHFL_SELECTION_ERROR = 4
    enumerator :: CHFL_CONFIGURATION_ERROR = 5
    enumerator :: CHFL_OUT_OF_BOUNDS = 6
    enumerator :: CHFL_PROPERTY_ERROR = 7
    enumerator :: CHFL_GENERIC_ERROR = 254
    enumerator :: CHFL_CXX_ERROR = 255
end enum

integer, parameter :: chfl_status = kind(CHFL_SUCCESS)

enum, bind(C)
    enumerator :: CHFL_BOND_UNKNOWN = 0
    enumerator :: CHFL_BOND_SINGLE = 1
    enumerator :: CHFL_BOND_DOUBLE = 2
    enumerator :: CHFL_BOND_TRIPLE = 3
    enumerator :: CHFL_BOND_QUADRUPLE = 4
    enumerator :: CHFL_BOND_QINTUPLET = 5
    enumerator :: CHFL_BOND_AMIDE = 254
    enumerator :: CHFL_BOND_AROMATIC = 255
end enum

integer, parameter :: chfl_bond_order = kind(CHFL_BOND_UNKNOWN)

enum, bind(C)
    enumerator :: CHFL_PROPERTY_BOOL = 0
    enumerator :: CHFL_PROPERTY_DOUBLE = 1
    enumerator :: CHFL_PROPERTY_STRING = 2
    enumerator :: CHFL_PROPERTY_VECTOR3D = 3
end enum

integer, parameter :: chfl_property_kind = kind(CHFL_PROPERTY_BOOL)

enum, bind(C)
    enumerator :: CHFL_CELL_ORTHORHOMBIC = 0
    enumerator :: CHFL_CELL_TRICLINIC = 1
    enumerator :: CHFL_CELL_INFINITE = 2
end enum

integer, parameter :: chfl_cellshape = kind(CHFL_CELL_ORTHORHOMBIC)
