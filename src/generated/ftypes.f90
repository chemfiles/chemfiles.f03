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


type chfl_topology
    private
    type(c_ptr) :: ptr
contains
    procedure :: init => chfl_topology_init_
    procedure :: from_frame => chfl_topology_from_frame_init_
    procedure :: atoms_count => chfl_topology_atoms_count
    procedure :: append => chfl_topology_append
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
    procedure :: free => chfl_topology_free
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
    procedure :: set_topology_file => chfl_trajectory_set_topology_file
    procedure :: set_cell => chfl_trajectory_set_cell
    procedure :: nsteps => chfl_trajectory_nsteps
    procedure :: close => chfl_trajectory_close
end type

type chfl_frame
    private
    type(c_ptr) :: ptr
contains
    procedure :: init => chfl_frame_init_
    procedure :: atoms_count => chfl_frame_atoms_count
    procedure :: positions => chfl_frame_positions
    procedure :: set_positions => chfl_frame_set_positions
    procedure :: velocities => chfl_frame_velocities
    procedure :: set_velocities => chfl_frame_set_velocities
    procedure :: has_velocities => chfl_frame_has_velocities
    procedure :: set_cell => chfl_frame_set_cell
    procedure :: set_topology => chfl_frame_set_topology
    procedure :: step => chfl_frame_step
    procedure :: set_step => chfl_frame_set_step
    procedure :: guess_topology => chfl_frame_guess_topology
    procedure :: free => chfl_frame_free
end type

type chfl_atom
    private
    type(c_ptr) :: ptr
contains
    procedure :: init => chfl_atom_init_
    procedure :: from_frame => chfl_atom_from_frame_init_
    procedure :: from_topology => chfl_atom_from_topology_init_
    procedure :: mass => chfl_atom_mass
    procedure :: set_mass => chfl_atom_set_mass
    procedure :: charge => chfl_atom_charge
    procedure :: set_charge => chfl_atom_set_charge
    procedure :: name => chfl_atom_name
    procedure :: set_name => chfl_atom_set_name
    procedure :: full_name => chfl_atom_full_name
    procedure :: vdw_radius => chfl_atom_vdw_radius
    procedure :: covalent_radius => chfl_atom_covalent_radius
    procedure :: atomic_number => chfl_atom_atomic_number
    procedure :: type => chfl_atom_type
    procedure :: set_type => chfl_atom_set_type
    procedure :: free => chfl_atom_free
end type

type chfl_cell
    private
    type(c_ptr) :: ptr
contains
    procedure :: init => chfl_cell_init_
    procedure :: triclinic => chfl_cell_triclinic_init_
    procedure :: from_frame => chfl_cell_from_frame_init_
    procedure :: volume => chfl_cell_volume
    procedure :: lengths => chfl_cell_lengths
    procedure :: set_lengths => chfl_cell_set_lengths
    procedure :: angles => chfl_cell_angles
    procedure :: set_angles => chfl_cell_set_angles
    procedure :: matrix => chfl_cell_matrix
    procedure :: type => chfl_cell_type
    procedure :: set_type => chfl_cell_set_type
    procedure :: periodicity => chfl_cell_periodicity
    procedure :: set_periodicity => chfl_cell_set_periodicity
    procedure :: free => chfl_cell_free
end type
