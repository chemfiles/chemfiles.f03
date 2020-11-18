! chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux - BSD License

module chemfiles
    use chemfiles_ffi
    use chemfiles_strings
    use chemfiles_misc
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
    public :: chfl_format_metadata
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
    public :: CHFL_BOND_TRIPLE, CHFL_BOND_QUADRUPLE, CHFL_BOND_QUINTUPLET
    public :: CHFL_BOND_AMIDE, CHFL_BOND_AROMATIC
    ! Export free functions
    public :: chfl_version, chfl_set_warning_callback, chfl_add_configuration
    public :: chfl_last_error, chfl_clear_errors, chfl_formats_list
    ! Export others
    public :: CHFL_STRING_LENGTH

contains
    function chfl_version() result(string)
        implicit none

        character(len=CHFL_STRING_LENGTH) :: string
        type(c_ptr) :: c_string

        c_string = c_chfl_version()
        string = c_to_f_str(c_string)
    end function
end module
