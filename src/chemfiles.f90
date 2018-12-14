! chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux - BSD License

module chemfiles
    use chemfiles_cdef
    use chemfiles_strings
    use chemfiles_selection
    use chemfiles_property
    use chemfiles_atom
    use chemfiles_cell
    use chemfiles_residue
    use chemfiles_topology
    use chemfiles_frame
    use chemfiles_trajectory
    implicit none

    private
    ! Export types
    public :: chfl_selection, chfl_match, chfl_trajectory, chfl_property
    public :: chfl_frame, chfl_cell, chfl_topology, chfl_residue, chfl_atom
    ! Export enums
    public :: chfl_cellshape
    public :: CHFL_CELL_ORTHORHOMBIC, CHFL_CELL_TRICLINIC, CHFL_CELL_INFINITE
    public :: chfl_property_kind
    public :: CHFL_PROPERTY_DOUBLE, CHFL_PROPERTY_BOOL, CHFL_PROPERTY_STRING, CHFL_PROPERTY_VECTOR3D
    public :: chfl_status
    public :: CHFL_SUCCESS, CHFL_MEMORY_ERROR, CHFL_FILE_ERROR, CHFL_FORMAT_ERROR
    public :: CHFL_SELECTION_ERROR, CHFL_CONFIGURATION_ERROR, CHFL_OUT_OF_BOUNDS
    public :: CHFL_PROPERTY_ERROR, CHFL_GENERIC_ERROR, CHFL_CXX_ERROR
    public :: chfl_bond_order
    public :: CHFL_BOND_UNKNOWN, CHFL_BOND_SINGLE, CHFL_BOND_DOUBLE
    public :: CHFL_BOND_TRIPLE, CHFL_BOND_QUADRUPLE, CHFL_BOND_QINTUPLET
    public :: CHFL_BOND_AMIDE, CHFL_BOND_AROMATIC
    ! Export free functions
    public :: chfl_version, chfl_set_warning_callback, chfl_add_configuration
    public :: chfl_last_error, chfl_clear_errors
    ! Export others
    public :: CHFL_STRING_LENGTH

    ! Global pointer to the callback procedure
    procedure(chfl_warning_callback), pointer :: internal_warning_callback

    interface
        subroutine chfl_warning_callback(message)
            implicit none
            character(len=*), intent(in) :: message
        end subroutine chfl_warning_callback
    end interface

contains
    function chfl_version() result(string)
        implicit none

        character(len=CHFL_STRING_LENGTH) :: string
        type(c_ptr) :: c_string

        c_string = c_chfl_version()
        string = c_to_f_str(c_string)
    end function

    function chfl_last_error() result(string)
        implicit none

        character(len=CHFL_STRING_LENGTH) :: string
        type(c_ptr) :: c_string

        c_string = c_chfl_last_error()
        string = c_to_f_str(c_string)
    end function

    subroutine chfl_clear_errors(status)
        implicit none
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_clear_errors()

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine chfl_add_configuration(path, status)
        implicit none
        character(len=*), intent(in) :: path
        integer(chfl_status), optional :: status
        integer(chfl_status) :: status_tmp_

        status_tmp_ = c_chfl_add_configuration(f_to_c_str(path))

        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine

    subroutine internal_warning_wrapper(message) bind(C)
        implicit none
        type(c_ptr), intent(in), value :: message
        call internal_warning_callback(c_to_f_str(message))
    end subroutine

    subroutine chfl_set_warning_callback(callback, status)
        implicit none
        procedure(chfl_warning_callback) :: callback
        integer, optional :: status
        integer :: status_tmp_

        internal_warning_callback => callback
        status_tmp_ = c_chfl_set_warning_callback(c_funloc(internal_warning_wrapper))
        if (present(status)) then
            status = status_tmp_
        end if
    end subroutine
end module
