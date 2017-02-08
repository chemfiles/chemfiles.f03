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


subroutine chfl_cell_init_(this, lenghts, status)
    implicit none
    class(chfl_cell) :: this
    real(kind=c_double), dimension(3), intent(in) :: lenghts
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_cell(lenghts)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_triclinic_init_(this, lenghts, angles, status)
    implicit none
    class(chfl_cell) :: this
    real(kind=c_double), dimension(3), intent(in) :: lenghts
    real(kind=c_double), dimension(3), intent(in) :: angles
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_cell_triclinic(lenghts, angles)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_from_frame_init_(this, frame, status)
    implicit none
    class(chfl_cell) :: this
    class(chfl_frame), intent(in) :: frame
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_cell_from_frame(frame%ptr)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_copy_init_(this, cell, status)
    implicit none
    class(chfl_cell) :: this
    class(chfl_cell), intent(in) :: cell
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_cell_copy(cell%ptr)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_volume(this, volume, status)
    implicit none
    class(chfl_cell), intent(in) :: this
    real(kind=c_double) :: volume
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_cell_volume(this%ptr, volume)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_lengths(this, lengths, status)
    implicit none
    class(chfl_cell), intent(in) :: this
    real(kind=c_double), dimension(3) :: lengths
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_cell_lengths(this%ptr, lengths)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_set_lengths(this, lenghts, status)
    implicit none
    class(chfl_cell) :: this
    real(kind=c_double), dimension(3), intent(in) :: lenghts
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_cell_set_lengths(this%ptr, lenghts)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_angles(this, angles, status)
    implicit none
    class(chfl_cell), intent(in) :: this
    real(kind=c_double), dimension(3) :: angles
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_cell_angles(this%ptr, angles)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_set_angles(this, angles, status)
    implicit none
    class(chfl_cell) :: this
    real(kind=c_double), dimension(3), intent(in) :: angles
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_cell_set_angles(this%ptr, angles)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_matrix(this, matrix, status)
    implicit none
    class(chfl_cell), intent(in) :: this
    real(kind=c_double), dimension(3 ,3), target :: matrix
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_cell_matrix(this%ptr, c_loc(matrix))
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_shape(this, shape, status)
    implicit none
    class(chfl_cell), intent(in) :: this
    integer(chfl_cell_shape_t) :: shape
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_cell_shape(this%ptr, shape)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_set_shape(this, shape, status)
    implicit none
    class(chfl_cell) :: this
    integer(chfl_cell_shape_t), value :: shape
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_cell_set_shape(this%ptr, shape)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_free(this, status)
    implicit none
    class(chfl_cell) :: this
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_cell_free(this%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
    this%ptr = c_null_ptr
end subroutine
