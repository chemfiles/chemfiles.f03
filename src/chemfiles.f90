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
    use iso_fortran_env, only: int32

    use strings
    implicit none

    private
    ! Export types
    public :: chfl_trajectory, chfl_frame, chfl_cell, chfl_topology, chfl_atom
    public :: chfl_selection, chfl_match
    ! Export enums
    public:: CHFL_LOG_LEVEL, CHFL_LOG_ERROR, CHFL_LOG_WARNING, CHFL_LOG_INFO, CHFL_LOG_DEBUG
    public:: CHFL_CELL_TYPES, CHFL_CELL_ORTHORHOMBIC, CHFL_CELL_TRICLINIC, CHFL_CELL_INFINITE
    public:: CHFL_ATOM_TYPES, CHFL_ATOM_ELEMENT, CHFL_ATOM_COARSE_GRAINED, CHFL_ATOM_DUMMY, CHFL_ATOM_UNDEFINED
    ! Export free functions
    public :: chfl_version, chfl_log_stderr, chfl_log_stdout, chfl_log_silent
    public :: chfl_logfile, chfl_loglevel, chfl_set_loglevel, chfl_log_callback
    public :: chfl_last_error, chfl_strerror, chfl_clear_errors

    ! Global pointer to the callback procedure
    procedure(chfl_logging_callback), pointer :: logging_callback

    type, bind(C) :: chfl_match
        integer(kind=c_signed_char) :: size
        integer(kind=c_size_t), dimension(4) :: atoms
    end type

    include "generated/cenums.f90"
    include "generated/cdef.f90"
    include "generated/ftypes.f90"

    interface
        ! Fortran callback
        subroutine chfl_logging_callback(level, message)
            import CHFL_LOG_LEVEL
            implicit none
            integer(kind=kind(CHFL_LOG_LEVEL)), intent(in) :: level
            character(len=*), intent(in) :: message
        end subroutine chfl_logging_callback

        ! C callback typedef
        subroutine chfl_logging_callback_c(level, message) bind(C)
            use iso_c_binding
            implicit none
            integer(c_int), intent(in), value :: level
            type(c_ptr), intent(in), value :: message
        end subroutine chfl_logging_callback_c
    end interface

contains
    include "generated/interface.f90"

    subroutine logging_callback_wrapper(level, message) bind(C)
        implicit none
        integer(c_int), intent(in), value :: level
        type(c_ptr), intent(in), value :: message

        call logging_callback(level, c_to_f_str(message))
    end subroutine logging_callback_wrapper

    subroutine chfl_log_callback(callback, status)
        implicit none
        procedure(chfl_logging_callback) :: callback
        integer, optional :: status
        integer :: status_tmp_

        logging_callback => callback
        status_tmp_ = chfl_log_callback_c(c_funloc(logging_callback_wrapper))
        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine
end module chemfiles
