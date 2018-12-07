! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_trajectory
    use iso_c_binding
    use chemfiles_cdef
    use chemfiles_strings
    use chemfiles_cell
    use chemfiles_frame
    use chemfiles_topology
    implicit none

    private
    public :: chfl_trajectory

    type chfl_trajectory
        private
        type(c_ptr) :: ptr = c_null_ptr
    contains
        procedure :: open => chfl_trajectory_open_init_
        procedure :: with_format => chfl_trajectory_with_format_init_
        procedure :: path => chfl_trajectory_path
        procedure :: read => chfl_trajectory_read
        procedure :: read_step => chfl_trajectory_read_step
        procedure :: write => chfl_trajectory_write
        procedure :: set_topology => chfl_trajectory_set_topology
        procedure :: topology_file => chfl_trajectory_topology_file
        procedure :: set_cell => chfl_trajectory_set_cell
        procedure :: nsteps => chfl_trajectory_nsteps
        procedure :: close => chfl_trajectory_close
    end type

contains
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

    subroutine chfl_trajectory_path(this, path, status)
        implicit none
        class(chfl_trajectory), intent(in) :: this
        character, intent(in), dimension(:), pointer :: path
        integer(chfl_status), optional :: status
        type(c_ptr), target :: c_path_
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_trajectory_path(this%ptr, c_loc(c_path_))

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

        status_tmp_ = c_chfl_trajectory_read(this%ptr, frame%unsafe_ptr())

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

        status_tmp_ = c_chfl_trajectory_read_step(this%ptr, step, frame%unsafe_ptr())

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

        status_tmp_ = c_chfl_trajectory_write(this%ptr, frame%unsafe_const_ptr())

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

        status_tmp_ = c_chfl_trajectory_set_topology(this%ptr, topology%unsafe_const_ptr())

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

        status_tmp_ = c_chfl_trajectory_set_cell(this%ptr, cell%unsafe_const_ptr())

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
        class(chfl_trajectory), intent(in) :: this
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_trajectory_close(this%ptr)

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine
end module
