! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_cell
    use iso_c_binding
    use chemfiles_ffi
    implicit none

    private
    public :: chfl_cell

    type, extends(chfl_ptr) :: chfl_cell
    contains
        procedure :: init
        procedure :: copy
        procedure :: volume
        procedure :: lengths
        procedure :: set_lengths
        procedure :: angles
        procedure :: set_angles
        procedure :: matrix
        procedure :: shape => get_shape
        procedure :: set_shape
        procedure :: wrap
    end type

contains
    subroutine init(this, lengths, angles, status)
        implicit none
        class(chfl_cell), intent(inout) :: this
        real(kind=c_double), dimension(3), intent(in) :: lengths
        real(kind=c_double), dimension(3), intent(in), optional :: angles
        integer(chfl_status), intent(out), optional :: status

        if (present(angles)) then
            call this%unsafe_set_ptr(c_chfl_cell(lengths, angles), status)
        else
            call this%unsafe_set_ptr(c_chfl_cell(lengths, [90d0, 90d0, 90d0]), status)
        end if
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

        status_tmp = c_chfl_cell_set_shape(                                    &
            this%unsafe_ptr(), chfl_cellshape_from_integer(shape)              &
        )

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
end module
