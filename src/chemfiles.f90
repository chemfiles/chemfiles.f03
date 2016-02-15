! chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015 Guillaume Fraux
!
! This Source Code Form is subject to the terms of the Mozilla Public
! License, v. 2.0. If a copy of the MPL was not distributed with this
! file, You can obtain one at http://mozilla.org/MPL/2.0/
!
! This is the main module file for the Fortran interface to chemfiles.

module chemfiles
    use iso_c_binding
    use strings
    implicit none
    private
    ! Export types
    public :: chfl_trajectory, chfl_frame, chfl_cell, chfl_topology, chfl_atom
    ! Export enums
    public:: CHFL_LOG_LEVEL, CHFL_LOG_ERROR, CHFL_LOG_WARNING, CHFL_LOG_INFO, CHFL_LOG_DEBUG
    public:: CHFL_CELL_TYPES, CHFL_CELL_ORTHOROMBIC, CHFL_CELL_TRICLINIC, CHFL_CELL_INFINITE
    public:: CHFL_ATOM_TYPES, CHFL_ATOM_ELEMENT, CHFL_ATOM_COARSE_GRAINED, CHFL_ATOM_DUMMY, CHFL_ATOM_UNDEFINED
    ! Export free functions
    public :: chfl_version, chfl_log_stderr, chfl_log_stdout, chfl_log_silent
    public :: chfl_logfile, chfl_loglevel, chfl_set_loglevel
    public :: chfl_last_error, chfl_strerror

    include "generated/cenums.f90"
    include "generated/cdef.f90"
    include "generated/ftypes.f90"
contains
    include "generated/interface.f90"
end module chemfiles
