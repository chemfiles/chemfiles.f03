! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_trajectory
    use iso_c_binding
    use chemfiles_ffi
    use chemfiles_strings
    use chemfiles_cell
    use chemfiles_frame
    use chemfiles_topology
    implicit none

    private
    public :: chfl_trajectory

    type, extends(chfl_ptr) :: chfl_trajectory
    contains
        procedure :: open
        procedure :: path
        procedure :: read
        procedure :: read_step
        procedure :: write
        procedure :: set_topology
        procedure :: topology_file
        procedure :: set_cell
        procedure :: nsteps
        procedure :: close
    end type

contains
    subroutine open(this, path, mode, format, status)
        implicit none
        class(chfl_trajectory), intent(inout) :: this
        character(len=*), intent(in) :: path
        character, value :: mode
        character(len=*), intent(in), optional :: format
        integer(chfl_status), intent(out), optional :: status

        if (present(format)) then
            call this%unsafe_set_ptr(                                                      &
                c_chfl_trajectory_with_format(f_to_c_str(path), mode, f_to_c_str(format)), &
                status=status                                                              &
            )
        else
            call this%unsafe_set_ptr(c_chfl_trajectory_open(f_to_c_str(path), mode), status)
        end if
    end subroutine

    character(len=CHFL_STRING_LENGTH) function path(this, status)
        implicit none
        class(chfl_trajectory), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_trajectory_path(this%unsafe_const_ptr(), path, int(CHFL_STRING_LENGTH, c_int64_t))
        path = rm_null_in_str(path)
        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine read(this, frame, status)
        implicit none
        class(chfl_trajectory), intent(inout) :: this
        type(chfl_frame), intent(inout) :: frame
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_trajectory_read(this%unsafe_ptr(), frame%unsafe_ptr())

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine read_step(this, step, frame, status)
        implicit none
        class(chfl_trajectory), intent(inout) :: this
        integer(kind=c_int64_t), intent(in) :: step
        type(chfl_frame) :: frame
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_trajectory_read_step(this%unsafe_ptr(), step, frame%unsafe_ptr())

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine write(this, frame, status)
        implicit none
        class(chfl_trajectory), intent(inout) :: this
        type(chfl_frame), intent(in) :: frame
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_trajectory_write(this%unsafe_ptr(), frame%unsafe_const_ptr())

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine set_topology(this, topology, status)
        implicit none
        class(chfl_trajectory), intent(inout) :: this
        type(chfl_topology), intent(in) :: topology
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_trajectory_set_topology(this%unsafe_ptr(), topology%unsafe_const_ptr())

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine topology_file(this, path, format, status)
        implicit none
        class(chfl_trajectory), intent(inout) :: this
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: format
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_trajectory_topology_file(this%unsafe_ptr(), f_to_c_str(path), f_to_c_str(format))

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine set_cell(this, cell, status)
        implicit none
        class(chfl_trajectory), intent(inout) :: this
        type(chfl_cell), intent(in) :: cell
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_trajectory_set_cell(this%unsafe_ptr(), cell%unsafe_const_ptr())

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    integer(kind=c_int64_t) function nsteps(this, status)
        implicit none
        class(chfl_trajectory), intent(inout) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_trajectory_nsteps(this%unsafe_ptr(), nsteps)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine close(this)
        implicit none
        class(chfl_trajectory), intent(inout) :: this

        call this%free()
    end subroutine
end module
