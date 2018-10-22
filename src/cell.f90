! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_cell
    use iso_c_binding
    use chemfiles_cdef
    implicit none

    private
    public :: chfl_cell

    type chfl_cell
        private
        type(c_ptr) :: ptr = c_null_ptr
    contains
        procedure :: unsafe_set_ptr
        procedure :: unsafe_ptr
        procedure :: unsafe_const_ptr

        generic, public :: init => orthorhombic, triclinic, copy
        procedure, private :: orthorhombic, triclinic, copy

        procedure :: volume
        procedure :: lengths
        procedure :: set_lengths
        procedure :: angles
        procedure :: set_angles
        procedure :: matrix
        procedure :: shape => get_shape
        procedure :: set_shape
        procedure :: wrap
        procedure :: free
    end type

contains
    subroutine unsafe_set_ptr(this, ptr, status)
        implicit none
        class(chfl_cell), intent(inout) :: this
        type(c_ptr), intent(in) :: ptr
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: dummy

        if (c_associated(this%unsafe_ptr())) then
            print*, "Trying to reset an allocated chfl_cell. Call chfl_cell%free first."
            ! free the allocated memory
            dummy = c_chfl_cell_free(ptr)
            if (present(status)) then
                status = CHFL_MEMORY_ERROR
            end if
            return
        end if

        this%ptr = ptr

        if (present(status)) then
            if (.not. c_associated(this%unsafe_ptr())) then
                status = CHFL_MEMORY_ERROR
            else
                status = CHFL_SUCCESS
            end if
        end if
    end subroutine

    type(c_ptr) function unsafe_ptr(this)
        implicit none
        class(chfl_cell), intent(inout) :: this
        unsafe_ptr = this%ptr
    end function

    type(c_ptr) function unsafe_const_ptr(this)
        implicit none
        class(chfl_cell), intent(in) :: this
        unsafe_const_ptr = this%ptr
    end function

    subroutine orthorhombic(this, lengths, status)
        implicit none
        class(chfl_cell), intent(inout) :: this
        real(kind=c_double), dimension(3), intent(in) :: lengths
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(c_chfl_cell(lengths), status)
    end subroutine

    subroutine triclinic(this, lengths, angles, status)
        implicit none
        class(chfl_cell), intent(inout) :: this
        real(kind=c_double), dimension(3), intent(in) :: lengths
        real(kind=c_double), dimension(3), intent(in) :: angles
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(c_chfl_cell_triclinic(lengths, angles), status)
    end subroutine

    subroutine copy(this, cell, status)
        implicit none
        class(chfl_cell), intent(inout) :: this
        type(chfl_cell), intent(in) :: cell
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(c_chfl_cell_copy(cell%unsafe_const_ptr()), status)
    end subroutine

    real(kind=c_double) function volume(this, status)
        implicit none
        class(chfl_cell), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_cell_volume(this%unsafe_const_ptr(), volume)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    function lengths(this, status)
        implicit none
        class(chfl_cell), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        real(kind=c_double), dimension(3) :: lengths
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_cell_lengths(this%unsafe_const_ptr(), lengths)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine set_lengths(this, lengths, status)
        implicit none
        class(chfl_cell), intent(inout) :: this
        real(kind=c_double), dimension(3), intent(in) :: lengths
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_cell_set_lengths(this%unsafe_ptr(), lengths)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    function angles(this, status)
        implicit none
        class(chfl_cell), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        real(kind=c_double), dimension(3) :: angles
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_cell_angles(this%unsafe_const_ptr(), angles)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine set_angles(this, angles, status)
        implicit none
        class(chfl_cell), intent(inout) :: this
        real(kind=c_double), dimension(3), intent(in) :: angles
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_cell_set_angles(this%unsafe_ptr(), angles)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    function matrix(this, status)
        implicit none
        class(chfl_cell), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        real(kind=c_double), dimension(3 ,3), target :: matrix
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_cell_matrix(this%unsafe_const_ptr(), c_loc(matrix))

        if (present(status)) then
            status = status_tmp
        end if
    end function

    integer(chfl_cellshape) function get_shape(this, status)
        implicit none
        class(chfl_cell), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_cell_shape(this%unsafe_const_ptr(), get_shape)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine set_shape(this, shape, status)
        implicit none
        class(chfl_cell), intent(inout) :: this
        integer(chfl_cellshape), intent(in) :: shape
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_cell_set_shape(this%unsafe_ptr(), shape)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine wrap(this, vector, status)
        implicit none
        class(chfl_cell), intent(in) :: this
        real(kind=c_double), dimension(3), intent(inout) :: vector
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_cell_wrap(this%unsafe_const_ptr(), vector)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine free(this, status)
        implicit none
        class(chfl_cell), intent(inout) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_cell_free(this%unsafe_ptr())
        this%ptr = c_null_ptr

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine
end module
