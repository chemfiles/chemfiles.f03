! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015 Guillaume Fraux
!
! This Source Code Form is subject to the terms of the Mozilla Public
! License, v. 2.0. If a copy of the MPL was not distributed with this
! file, You can obtain one at http://mozilla.org/MPL/2.0/
!
! =========================================================================== !
! !!!! AUTO-GENERATED FILE !!!! Do not edit. See bindgen repository for the
! generating code (https://github.com/chemfiles/bindgen).
! This file contains Fortran 2003 ISO C Binding interface to the C API
!
! This file is not compilable on his own, but should be 'include'd in another
! fortran compilation unit.
! =========================================================================== !


type chfl_atom
    private
    type(c_ptr) :: ptr
contains
    procedure :: init => chfl_atom_init_
    procedure :: copy => chfl_atom_copy_init_
    procedure :: from_frame => chfl_atom_from_frame_init_
    procedure :: from_topology => chfl_atom_from_topology_init_
    procedure :: mass => chfl_atom_mass
    procedure :: set_mass => chfl_atom_set_mass
    procedure :: charge => chfl_atom_charge
    procedure :: set_charge => chfl_atom_set_charge
    procedure :: type => chfl_atom_type
    procedure :: set_type => chfl_atom_set_type
    procedure :: name => chfl_atom_name
    procedure :: set_name => chfl_atom_set_name
    procedure :: full_name => chfl_atom_full_name
    procedure :: vdw_radius => chfl_atom_vdw_radius
    procedure :: covalent_radius => chfl_atom_covalent_radius
    procedure :: atomic_number => chfl_atom_atomic_number
    procedure :: free => chfl_atom_free
end type

type chfl_residue
    private
    type(c_ptr) :: ptr
contains
    procedure :: init => chfl_residue_init_
    procedure :: from_topology => chfl_residue_from_topology_init_
    procedure :: for_atom => chfl_residue_for_atom_init_
    procedure :: copy => chfl_residue_copy_init_
    procedure :: atoms_count => chfl_residue_atoms_count
    procedure :: id => chfl_residue_id
    procedure :: name => chfl_residue_name
    procedure :: add_atom => chfl_residue_add_atom
    procedure :: contains => chfl_residue_contains
    procedure :: free => chfl_residue_free
end type

type chfl_topology
    private
    type(c_ptr) :: ptr
contains
    procedure :: init => chfl_topology_init_
    procedure :: from_frame => chfl_topology_from_frame_init_
    procedure :: copy => chfl_topology_copy_init_
    procedure :: atoms_count => chfl_topology_atoms_count
    procedure :: resize => chfl_topology_resize
    procedure :: add_atom => chfl_topology_add_atom
    procedure :: remove => chfl_topology_remove
    procedure :: isbond => chfl_topology_isbond
    procedure :: isangle => chfl_topology_isangle
    procedure :: isdihedral => chfl_topology_isdihedral
    procedure :: bonds_count => chfl_topology_bonds_count
    procedure :: angles_count => chfl_topology_angles_count
    procedure :: dihedrals_count => chfl_topology_dihedrals_count
    procedure :: bonds => chfl_topology_bonds
    procedure :: angles => chfl_topology_angles
    procedure :: dihedrals => chfl_topology_dihedrals
    procedure :: add_bond => chfl_topology_add_bond
    procedure :: remove_bond => chfl_topology_remove_bond
    procedure :: residues_count => chfl_topology_residues_count
    procedure :: add_residue => chfl_topology_add_residue
    procedure :: residues_linked => chfl_topology_residues_linked
    procedure :: free => chfl_topology_free
end type

type chfl_cell
    private
    type(c_ptr) :: ptr
contains
    procedure :: init => chfl_cell_init_
    procedure :: triclinic => chfl_cell_triclinic_init_
    procedure :: from_frame => chfl_cell_from_frame_init_
    procedure :: copy => chfl_cell_copy_init_
    procedure :: volume => chfl_cell_volume
    procedure :: lengths => chfl_cell_lengths
    procedure :: set_lengths => chfl_cell_set_lengths
    procedure :: angles => chfl_cell_angles
    procedure :: set_angles => chfl_cell_set_angles
    procedure :: matrix => chfl_cell_matrix
    procedure :: shape => chfl_cell_shape
    procedure :: set_shape => chfl_cell_set_shape
    procedure :: free => chfl_cell_free
end type

type chfl_frame
    private
    type(c_ptr) :: ptr
contains
    procedure :: init => chfl_frame_init_
    procedure :: copy => chfl_frame_copy_init_
    procedure :: atoms_count => chfl_frame_atoms_count
    procedure :: positions => chfl_frame_positions
    procedure :: velocities => chfl_frame_velocities
    procedure :: add_atom => chfl_frame_add_atom
    procedure :: remove => chfl_frame_remove
    procedure :: resize => chfl_frame_resize
    procedure :: add_velocities => chfl_frame_add_velocities
    procedure :: has_velocities => chfl_frame_has_velocities
    procedure :: set_cell => chfl_frame_set_cell
    procedure :: set_topology => chfl_frame_set_topology
    procedure :: step => chfl_frame_step
    procedure :: set_step => chfl_frame_set_step
    procedure :: guess_topology => chfl_frame_guess_topology
    procedure :: free => chfl_frame_free
end type

type chfl_trajectory
    private
    type(c_ptr) :: ptr
contains
    procedure :: open => chfl_trajectory_open_init_
    procedure :: with_format => chfl_trajectory_with_format_init_
    procedure :: read => chfl_trajectory_read
    procedure :: read_step => chfl_trajectory_read_step
    procedure :: write => chfl_trajectory_write
    procedure :: set_topology => chfl_trajectory_set_topology
    procedure :: topology_file => chfl_trajectory_topology_file
    procedure :: set_cell => chfl_trajectory_set_cell
    procedure :: nsteps => chfl_trajectory_nsteps
    procedure :: close => chfl_trajectory_close
end type

type chfl_selection
    private
    type(c_ptr) :: ptr
contains
    procedure :: init => chfl_selection_init_
    procedure :: copy => chfl_selection_copy_init_
    procedure :: size => chfl_selection_size
    procedure :: string => chfl_selection_string
    procedure :: evaluate => chfl_selection_evaluate
    procedure :: matches => chfl_selection_matches
    procedure :: free => chfl_selection_free
end type
