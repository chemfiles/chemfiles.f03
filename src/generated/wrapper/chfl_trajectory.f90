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


subroutine chfl_trajectory_open_init_(this, path, mode, status)
    implicit none
    class(chfl_trajectory) :: this
    character(len=*), intent(in) :: path
    character, value :: mode
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_trajectory_open(f_to_c_str(path), mode)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_with_format_init_(this, path, mode, format, status)
    implicit none
    class(chfl_trajectory) :: this
    character(len=*), intent(in) :: path
    character, value :: mode
    character(len=*), intent(in) :: format
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_trajectory_with_format(f_to_c_str(path), mode, f_to_c_str(format))

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_read(this, frame, status)
    implicit none
    class(chfl_trajectory) :: this
    class(chfl_frame) :: frame
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_trajectory_read(this%ptr, frame%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_read_step(this, step, frame, status)
    implicit none
    class(chfl_trajectory) :: this
    integer(kind=c_int64_t), value :: step
    class(chfl_frame) :: frame
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_trajectory_read_step(this%ptr, step, frame%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_write(this, frame, status)
    implicit none
    class(chfl_trajectory) :: this
    class(chfl_frame), intent(in) :: frame
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_trajectory_write(this%ptr, frame%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_set_topology(this, topology, status)
    implicit none
    class(chfl_trajectory) :: this
    class(chfl_topology), intent(in) :: topology
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_trajectory_set_topology(this%ptr, topology%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_topology_file(this, path, format, status)
    implicit none
    class(chfl_trajectory) :: this
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: format
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_trajectory_topology_file(this%ptr, f_to_c_str(path), f_to_c_str(format))
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_set_cell(this, cell, status)
    implicit none
    class(chfl_trajectory) :: this
    class(chfl_cell), intent(in) :: cell
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_trajectory_set_cell(this%ptr, cell%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_nsteps(this, nsteps, status)
    implicit none
    class(chfl_trajectory) :: this
    integer(kind=c_int64_t) :: nsteps
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_trajectory_nsteps(this%ptr, nsteps)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_trajectory_close(this, status)
    implicit none
    class(chfl_trajectory) :: this
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_trajectory_close(this%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
    this%ptr = c_null_ptr
end subroutine
