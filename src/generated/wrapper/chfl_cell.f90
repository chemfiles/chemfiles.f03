! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2017 Guillaume Fraux -- BSD licence
!
! =========================================================================== !
! !!!! AUTO-GENERATED FILE !!!! Do not edit. See bindgen repository for the
! generating code (https://github.com/chemfiles/bindgen).
! This file contains Fortran 2003 ISO C Binding interface to the C API
!
! This file is not compilable on his own, but should be 'include'd in another
! fortran compilation unit.
! =========================================================================== !


subroutine chfl_cell_init_(this, lengths, status)
    implicit none
    class(chfl_cell) :: this
    real(kind=c_double), dimension(3), intent(in) :: lengths
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_cell(lengths)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_triclinic_init_(this, lengths, angles, status)
    implicit none
    class(chfl_cell) :: this
    real(kind=c_double), dimension(3), intent(in) :: lengths
    real(kind=c_double), dimension(3), intent(in) :: angles
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_cell_triclinic(lengths, angles)

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

subroutine chfl_cell_set_lengths(this, lengths, status)
    implicit none
    class(chfl_cell) :: this
    real(kind=c_double), dimension(3), intent(in) :: lengths
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_cell_set_lengths(this%ptr, lengths)
    
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
    integer(chfl_cellshape) :: shape
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
    integer(chfl_cellshape), value :: shape
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_cell_set_shape(this%ptr, shape)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_cell_wrap(this, vector, status)
    implicit none
    class(chfl_cell), intent(in) :: this
    real(kind=c_double), dimension(3) :: vector
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_cell_wrap(this%ptr, vector)
    
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
