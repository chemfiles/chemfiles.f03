! chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence

module chemfiles_ffi
    use iso_c_binding
    implicit none

    type, bind(C) :: chfl_match
        integer(kind=c_size_t) :: size
        integer(kind=c_size_t), dimension(4) :: atoms
    end type

    type, bind(C) :: c_chfl_format_metadata
        type(c_ptr) :: name
        type(c_ptr) :: extension
        type(c_ptr) :: description
        type(c_ptr) :: reference

        logical(kind=c_bool) :: read
        logical(kind=c_bool) :: write
        logical(kind=c_bool) :: memory

        logical(kind=c_bool) :: positions
        logical(kind=c_bool) :: velocities
        logical(kind=c_bool) :: unit_cell
        logical(kind=c_bool) :: atoms
        logical(kind=c_bool) :: bonds
        logical(kind=c_bool) :: residues
    end type

    include "generated/enums.f90"
    include "generated/misc.f90"
    include "generated/chfl_atom.f90"
    include "generated/chfl_residue.f90"
    include "generated/chfl_topology.f90"
    include "generated/chfl_cell.f90"
    include "generated/chfl_frame.f90"
    include "generated/chfl_trajectory.f90"
    include "generated/chfl_selection.f90"
    include "generated/chfl_property.f90"

    interface
        ! C function interface definition for the warning callbacks
        subroutine chfl_warning_callback_c(message) bind(C)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: message
        end subroutine chfl_warning_callback_c

        ! Manual interface definition for chfl_free
        subroutine c_chfl_free(object) bind(C, name="chfl_free")
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: object
        end subroutine c_chfl_free

        ! Manual interface definition for the free stdlib function
        subroutine c_free(object) bind(C, name="free")
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in), value :: object
        end subroutine c_free
    end interface

    type chfl_ptr
        private
        type(c_ptr) :: ptr = c_null_ptr
        logical :: is_const = .false.
    contains
        procedure :: unsafe_set_ptr
        procedure :: unsafe_set_const_ptr
        procedure :: unsafe_ptr
        procedure :: unsafe_const_ptr
        procedure :: free => chfl_free
    end type

contains

    subroutine unsafe_set_ptr(this, ptr, status)
        implicit none
        class(chfl_ptr), intent(inout) :: this
        type(c_ptr), intent(in) :: ptr
        integer(chfl_status), intent(out), optional :: status

        if (c_associated(this%ptr)) then
            write(*,*) "Trying to reset an allocated pointer."
            ! free the allocated memory
            call c_chfl_free(ptr)
            if (present(status)) then
                status = CHFL_MEMORY_ERROR
            end if
            return
        end if

        this%ptr = ptr

        if (present(status)) then
            if (.not. c_associated(this%ptr)) then
                status = CHFL_MEMORY_ERROR
            else
                status = CHFL_SUCCESS
            end if
        end if
    end subroutine

    subroutine unsafe_set_const_ptr(this, ptr, status)
        implicit none
        class(chfl_ptr), intent(inout) :: this
        type(c_ptr), intent(in) :: ptr
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(ptr, status)
        this%is_const = .true.
    end subroutine

    type(c_ptr) function unsafe_ptr(this)
        implicit none
        class(chfl_ptr), intent(inout) :: this

        if (.not. c_associated(this%ptr)) then
            write(*, *) "Trying to access a NULL pointer. Call <object>%init first."
        elseif (this%is_const) then
            write(*, *) "Can not write data to a const pointer"
            stop 1
        end if
        unsafe_ptr = this%ptr
    end function

    type(c_ptr) function unsafe_const_ptr(this)
        implicit none
        class(chfl_ptr), intent(in) :: this

        if (.not. c_associated(this%ptr)) then
            write(*, *) "Trying to access a NULL chfl_residue. Call chfl_residue%init first."
        end if
        unsafe_const_ptr = this%ptr
    end function

    subroutine chfl_free(this)
        implicit none
        class(chfl_ptr), intent(inout) :: this

        call c_chfl_free(this%ptr)
        this%ptr = c_null_ptr
    end subroutine

    function chfl_cellshape_from_integer(value) result(output)
        integer(chfl_cellshape), intent(in) :: value
        integer(chfl_cellshape) :: output

        if (value == CHFL_CELL_INFINITE) then
            output = value
        else if (value == CHFL_CELL_ORTHORHOMBIC) then
            output = value
        else if (value == CHFL_CELL_TRICLINIC) then
            output = value
        else
            write(*, *) "got an invalid value for chfl_cellshape enum ", value, " defaulting to CHFL_CELL_INFINITE"
            output = CHFL_CELL_INFINITE
        end if
    end function

    function chfl_bond_order_from_integer(value) result(output)
        integer(chfl_bond_order), intent(in) :: value
        integer(chfl_bond_order) :: output

        if (value == CHFL_BOND_UNKNOWN) then
            output = value
        else if (value == CHFL_BOND_SINGLE) then
            output = value
        else if (value == CHFL_BOND_DOUBLE) then
            output = value
        else if (value == CHFL_BOND_TRIPLE) then
            output = value
        else if (value == CHFL_BOND_QUADRUPLE) then
            output = value
        else if (value == CHFL_BOND_QUINTUPLET) then
            output = value
        else if (value == CHFL_BOND_AMIDE) then
            output = value
        else if (value == CHFL_BOND_AROMATIC) then
            output = value
        else
            write(*, *) "got an invalid value for chfl_bond_order enum ", value, " defaulting to CHFL_BOND_UNKNOWN"
            output = CHFL_BOND_UNKNOWN
        end if
    end function
end module
