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
    public :: chfl_selection, chfl_match, chfl_trajectory
    public :: chfl_frame, chfl_cell, chfl_topology, chfl_residue, chfl_atom
    ! Export enums
    public :: chfl_cell_shape_t, CHFL_CELL_ORTHORHOMBIC, CHFL_CELL_TRICLINIC, CHFL_CELL_INFINITE
    public :: chfl_status, CHFL_SUCCESS, CHFL_MEMORY_ERROR, CHFL_FILE_ERROR
    public :: CHFL_FORMAT_ERROR, CHFL_SELECTION_ERROR, CHFL_GENERIC_ERROR, CHFL_CXX_ERROR
    ! Export free functions
    public :: chfl_version, chfl_set_warning_callback
    public :: chfl_last_error, chfl_clear_errors

    ! Global pointer to the callback procedure
    procedure(chfl_warning_callback), pointer :: warning_callback

    type, bind(C) :: chfl_match
        integer(kind=c_size_t) :: size
        integer(kind=c_size_t), dimension(4) :: atoms
    end type


    include "generated/types.f90"
    include "generated/cenums.f90"
    include "generated/cdef/others.f90"
    include "generated/cdef/chfl_atom.f90"
    include "generated/cdef/chfl_residue.f90"
    include "generated/cdef/chfl_topology.f90"
    include "generated/cdef/chfl_cell.f90"
    include "generated/cdef/chfl_frame.f90"
    include "generated/cdef/chfl_trajectory.f90"
    include "generated/cdef/chfl_selection.f90"

    interface
        ! Fortran callback
        subroutine chfl_warning_callback(message)
            implicit none
            character(len=*), intent(in) :: message
        end subroutine chfl_warning_callback

        ! C callback typedef
        subroutine chfl_warning_callback_c(message) bind(C)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: message
        end subroutine chfl_warning_callback_c
    end interface

contains
    include "generated/wrapper/others.f90"
    include "generated/wrapper/chfl_atom.f90"
    include "generated/wrapper/chfl_residue.f90"
    include "generated/wrapper/chfl_topology.f90"
    include "generated/wrapper/chfl_cell.f90"
    include "generated/wrapper/chfl_frame.f90"
    include "generated/wrapper/chfl_trajectory.f90"
    include "generated/wrapper/chfl_selection.f90"

    subroutine warning_callback_wrapper(message) bind(C)
        implicit none
        type(c_ptr), intent(in), value :: message
        call warning_callback(c_to_f_str(message))
    end subroutine warning_callback_wrapper

    subroutine chfl_set_warning_callback(callback, status)
        implicit none
        procedure(chfl_warning_callback) :: callback
        integer, optional :: status
        integer :: status_tmp_

        warning_callback => callback
        status_tmp_ = c_chfl_set_warning_callback(c_funloc(warning_callback_wrapper))
        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine chfl_set_warning_callback
end module
