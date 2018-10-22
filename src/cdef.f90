! chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence

module chemfiles_cdef
    use iso_c_binding
    implicit none

    type, bind(C) :: chfl_match
        integer(kind=c_size_t) :: size
        integer(kind=c_size_t), dimension(4) :: atoms
    end type

    include "generated/cenums.f90"
    include "generated/others.f90"
    include "generated/chfl_atom.f90"
    include "generated/chfl_residue.f90"
    include "generated/chfl_topology.f90"
    include "generated/chfl_cell.f90"
    include "generated/chfl_frame.f90"
    include "generated/chfl_trajectory.f90"
    include "generated/chfl_selection.f90"
    include "generated/chfl_property.f90"

    interface
        subroutine chfl_warning_callback_c(message) bind(C)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: message
        end subroutine chfl_warning_callback_c
    end interface
end module
