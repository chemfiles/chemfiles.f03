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

interface
! Function "chfl_cell", at cell.h:33:13
function c_chfl_cell(lenghts) bind(C, name="chfl_cell")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_cell
    real(kind=c_double), dimension(3), intent(in) :: lenghts
end function

! Function "chfl_cell_triclinic", at cell.h:50:13
function c_chfl_cell_triclinic(lenghts, angles) bind(C, name="chfl_cell_triclinic")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_cell_triclinic
    real(kind=c_double), dimension(3), intent(in) :: lenghts
    real(kind=c_double), dimension(3), intent(in) :: angles
end function

! Function "chfl_cell_from_frame", at cell.h:62:13
function c_chfl_cell_from_frame(frame) bind(C, name="chfl_cell_from_frame")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_cell_from_frame
    type(c_ptr), value, intent(in) :: frame
end function

! Function "chfl_cell_copy", at cell.h:72:13
function c_chfl_cell_copy(cell) bind(C, name="chfl_cell_copy")
    use iso_c_binding
    
    implicit none
    type(c_ptr) :: c_chfl_cell_copy
    type(c_ptr), value, intent(in) :: cell
end function

! Function "chfl_cell_volume", at cell.h:79:14
function c_chfl_cell_volume(cell, volume) bind(C, name="chfl_cell_volume")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_volume
    type(c_ptr), value, intent(in) :: cell
    real(kind=c_double) :: volume
end function

! Function "chfl_cell_lengths", at cell.h:88:14
function c_chfl_cell_lengths(cell, lengths) bind(C, name="chfl_cell_lengths")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_lengths
    type(c_ptr), value, intent(in) :: cell
    real(kind=c_double), dimension(3) :: lengths
end function

! Function "chfl_cell_set_lengths", at cell.h:99:14
function c_chfl_cell_set_lengths(cell, lenghts) bind(C, name="chfl_cell_set_lengths")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_set_lengths
    type(c_ptr), value :: cell
    real(kind=c_double), dimension(3), intent(in) :: lenghts
end function

! Function "chfl_cell_angles", at cell.h:108:14
function c_chfl_cell_angles(cell, angles) bind(C, name="chfl_cell_angles")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_angles
    type(c_ptr), value, intent(in) :: cell
    real(kind=c_double), dimension(3) :: angles
end function

! Function "chfl_cell_set_angles", at cell.h:121:14
function c_chfl_cell_set_angles(cell, angles) bind(C, name="chfl_cell_set_angles")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_set_angles
    type(c_ptr), value :: cell
    real(kind=c_double), dimension(3), intent(in) :: angles
end function

! Function "chfl_cell_matrix", at cell.h:139:14
function c_chfl_cell_matrix(cell, matrix) bind(C, name="chfl_cell_matrix")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_matrix
    type(c_ptr), value, intent(in) :: cell
    type(c_ptr), value :: matrix
end function

! Function "chfl_cell_shape", at cell.h:148:14
function c_chfl_cell_shape(cell, shape) bind(C, name="chfl_cell_shape")
    use iso_c_binding
    import chfl_status
import chfl_cellshape

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_shape
    type(c_ptr), value, intent(in) :: cell
    integer(chfl_cellshape) :: shape
end function

! Function "chfl_cell_set_shape", at cell.h:157:14
function c_chfl_cell_set_shape(cell, shape) bind(C, name="chfl_cell_set_shape")
    use iso_c_binding
    import chfl_status
import chfl_cellshape

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_set_shape
    type(c_ptr), value :: cell
    integer(chfl_cellshape), value :: shape
end function

! Function "chfl_cell_wrap", at cell.h:166:14
function c_chfl_cell_wrap(cell, vector) bind(C, name="chfl_cell_wrap")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_wrap
    type(c_ptr), value, intent(in) :: cell
    real(kind=c_double), dimension(3) :: vector
end function

! Function "chfl_cell_free", at cell.h:174:14
function c_chfl_cell_free(cell) bind(C, name="chfl_cell_free")
    use iso_c_binding
    import chfl_status

    implicit none
    integer(kind=chfl_status) :: c_chfl_cell_free
    type(c_ptr), value :: cell
end function

end interface
