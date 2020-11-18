! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
!
! =========================================================================== !
! !!!! AUTO-GENERATED FILE !!!! Do not edit. See bindgen repository for the
! generating code (https://github.com/chemfiles/bindgen).
! This file contains Fortran 2003 ISO C Binding interface to the C API
! =========================================================================== !

interface
! Function "chfl_cell", at cell.h:40
function c_chfl_cell(lengths, angles) bind(C, name="chfl_cell")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_cell
    real(kind=c_double), dimension(3), intent(in) :: lengths
    real(kind=c_double), dimension(3), intent(in) :: angles
end function

! Function "chfl_cell_from_matrix", at cell.h:55
function c_chfl_cell_from_matrix(matrix) bind(C, name="chfl_cell_from_matrix")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_cell_from_matrix
    type(c_ptr), value :: matrix
end function

! Function "chfl_cell_from_frame", at cell.h:68
function c_chfl_cell_from_frame(frame) bind(C, name="chfl_cell_from_frame")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_cell_from_frame
    type(c_ptr), value :: frame
end function

! Function "chfl_cell_copy", at cell.h:78
function c_chfl_cell_copy(cell) bind(C, name="chfl_cell_copy")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_cell_copy
    type(c_ptr), value, intent(in) :: cell
end function

! Function "chfl_cell_volume", at cell.h:85
function c_chfl_cell_volume(cell, volume) bind(C, name="chfl_cell_volume")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_volume
    type(c_ptr), value, intent(in) :: cell
    real(kind=c_double), intent(inout) :: volume
end function

! Function "chfl_cell_lengths", at cell.h:94
function c_chfl_cell_lengths(cell, lengths) bind(C, name="chfl_cell_lengths")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_lengths
    type(c_ptr), value, intent(in) :: cell
    real(kind=c_double), dimension(3), intent(inout) :: lengths
end function

! Function "chfl_cell_set_lengths", at cell.h:110
function c_chfl_cell_set_lengths(cell, lengths) bind(C, name="chfl_cell_set_lengths")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_set_lengths
    type(c_ptr), value :: cell
    real(kind=c_double), dimension(3), intent(in) :: lengths
end function

! Function "chfl_cell_angles", at cell.h:119
function c_chfl_cell_angles(cell, angles) bind(C, name="chfl_cell_angles")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_angles
    type(c_ptr), value, intent(in) :: cell
    real(kind=c_double), dimension(3), intent(inout) :: angles
end function

! Function "chfl_cell_set_angles", at cell.h:137
function c_chfl_cell_set_angles(cell, angles) bind(C, name="chfl_cell_set_angles")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_set_angles
    type(c_ptr), value :: cell
    real(kind=c_double), dimension(3), intent(in) :: angles
end function

! Function "chfl_cell_matrix", at cell.h:146
function c_chfl_cell_matrix(cell, matrix) bind(C, name="chfl_cell_matrix")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_matrix
    type(c_ptr), value, intent(in) :: cell
    type(c_ptr), value :: matrix
end function

! Function "chfl_cell_shape", at cell.h:155
function c_chfl_cell_shape(cell, shape) bind(C, name="chfl_cell_shape")
    use iso_c_binding
    import chfl_status
    import chfl_cellshape

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_shape
    type(c_ptr), value, intent(in) :: cell
    integer(chfl_cellshape), intent(inout) :: shape
end function

! Function "chfl_cell_set_shape", at cell.h:164
function c_chfl_cell_set_shape(cell, shape) bind(C, name="chfl_cell_set_shape")
    use iso_c_binding
    import chfl_status
    import chfl_cellshape

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_set_shape
    type(c_ptr), value :: cell
    integer(chfl_cellshape), value :: shape
end function

! Function "chfl_cell_wrap", at cell.h:173
function c_chfl_cell_wrap(cell, vector) bind(C, name="chfl_cell_wrap")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_wrap
    type(c_ptr), value, intent(in) :: cell
    real(kind=c_double), dimension(3), intent(inout) :: vector
end function

end interface
