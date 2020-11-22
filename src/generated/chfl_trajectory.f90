! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
!
! =========================================================================== !
! !!!! AUTO-GENERATED FILE !!!! Do not edit. See bindgen repository for the
! generating code (https://github.com/chemfiles/bindgen).
! This file contains Fortran 2003 ISO C Binding interface to the C API
! =========================================================================== !

interface
! Function "chfl_trajectory_open", at trajectory.h:26
function c_chfl_trajectory_open(path, mode) bind(C, name="chfl_trajectory_open")
    use iso_c_binding

    implicit none
    type(c_ptr) :: c_chfl_trajectory_open
    character(len=1, kind=c_char), dimension(*), intent(in) :: path
    character, value :: mode
end function

! Function "chfl_trajectory_with_format", at trajectory.h:43
function c_chfl_trajectory_with_format(path, mode, format) bind(C, name="chfl_trajectory_with_format")
    use iso_c_binding

    implicit none
    type(c_ptr) :: c_chfl_trajectory_with_format
    character(len=1, kind=c_char), dimension(*), intent(in) :: path
    character, value :: mode
    character(len=1, kind=c_char), dimension(*), intent(in) :: format
end function

! Function "chfl_trajectory_memory_reader", at trajectory.h:59
function c_chfl_trajectory_memory_reader(memory, size, format) bind(C, name="chfl_trajectory_memory_reader")
    use iso_c_binding

    implicit none
    type(c_ptr) :: c_chfl_trajectory_memory_reader
    character(len=1, kind=c_char), dimension(*), intent(in) :: memory
    integer(kind=c_int64_t), value :: size
    character(len=1, kind=c_char), dimension(*), intent(in) :: format
end function

! Function "chfl_trajectory_memory_writer", at trajectory.h:74
function c_chfl_trajectory_memory_writer(format) bind(C, name="chfl_trajectory_memory_writer")
    use iso_c_binding

    implicit none
    type(c_ptr) :: c_chfl_trajectory_memory_writer
    character(len=1, kind=c_char), dimension(*), intent(in) :: format
end function

! Function "chfl_trajectory_path", at trajectory.h:86
function c_chfl_trajectory_path(trajectory, path, buffsize) bind(C, name="chfl_trajectory_path")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_trajectory_path
    type(c_ptr), value, intent(in) :: trajectory
    character(len=1, kind=c_char), dimension(*) :: path
    integer(kind=c_int64_t), value :: buffsize
end function

! Function "chfl_trajectory_read", at trajectory.h:98
function c_chfl_trajectory_read(trajectory, frame) bind(C, name="chfl_trajectory_read")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_trajectory_read
    type(c_ptr), value :: trajectory
    type(c_ptr), value :: frame
end function

! Function "chfl_trajectory_read_step", at trajectory.h:110
function c_chfl_trajectory_read_step(trajectory, step, frame) bind(C, name="chfl_trajectory_read_step")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_trajectory_read_step
    type(c_ptr), value :: trajectory
    integer(kind=c_int64_t), value :: step
    type(c_ptr), value :: frame
end function

! Function "chfl_trajectory_write", at trajectory.h:119
function c_chfl_trajectory_write(trajectory, frame) bind(C, name="chfl_trajectory_write")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_trajectory_write
    type(c_ptr), value :: trajectory
    type(c_ptr), value, intent(in) :: frame
end function

! Function "chfl_trajectory_set_topology", at trajectory.h:130
function c_chfl_trajectory_set_topology(trajectory, topology) bind(C, name="chfl_trajectory_set_topology")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_trajectory_set_topology
    type(c_ptr), value :: trajectory
    type(c_ptr), value, intent(in) :: topology
end function

! Function "chfl_trajectory_topology_file", at trajectory.h:144
function c_chfl_trajectory_topology_file(trajectory, path, format) bind(C, name="chfl_trajectory_topology_file")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_trajectory_topology_file
    type(c_ptr), value :: trajectory
    character(len=1, kind=c_char), dimension(*), intent(in) :: path
    character(len=1, kind=c_char), dimension(*), intent(in) :: format
end function

! Function "chfl_trajectory_set_cell", at trajectory.h:154
function c_chfl_trajectory_set_cell(trajectory, cell) bind(C, name="chfl_trajectory_set_cell")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_trajectory_set_cell
    type(c_ptr), value :: trajectory
    type(c_ptr), value, intent(in) :: cell
end function

! Function "chfl_trajectory_nsteps", at trajectory.h:164
function c_chfl_trajectory_nsteps(trajectory, nsteps) bind(C, name="chfl_trajectory_nsteps")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_trajectory_nsteps
    type(c_ptr), value :: trajectory
    integer(kind=c_int64_t), intent(inout) :: nsteps
end function

! Function "chfl_trajectory_memory_buffer", at trajectory.h:178
function c_chfl_trajectory_memory_buffer(trajectory, data, size) bind(C, name="chfl_trajectory_memory_buffer")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_trajectory_memory_buffer
    type(c_ptr), value, intent(in) :: trajectory
    type(c_ptr), value :: data
    integer(kind=c_int64_t), intent(inout) :: size
end function

! Function "chfl_trajectory_close", at trajectory.h:188
function c_chfl_trajectory_close(trajectory) bind(C, name="chfl_trajectory_close")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_trajectory_close
    type(c_ptr), value, intent(in) :: trajectory
end function

end interface
