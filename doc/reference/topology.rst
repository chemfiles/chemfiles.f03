``chfl_topology`` type
----------------------

.. f:type:: chfl_topology

    A :f:type:`chfl_topology` contains the definition of all the atoms in the
    system, and the liaisons between the atoms (bonds, angles, dihedrals,
    ...). It will also contain all the residues information if it is available.

    The initialization routine for :f:type:`chfl_topology` are
    :f:func:`chfl_topology%init`, :f:func:`chfl_topology%from_frame` and
    :f:func:`chfl_topology%copy`.

    :field subroutine init: :f:func:`chfl_topology%init`
    :field subroutine copy: :f:func:`chfl_topology%copy`
    :field subroutine from_frame: :f:func:`chfl_topology%from_frame`
    :field subroutine atoms_count: :f:func:`chfl_topology%atoms_count`
    :field subroutine add_atom: :f:func:`chfl_topology%add_atom`
    :field subroutine resize: :f:func:`chfl_topology%resize`
    :field subroutine remove: :f:func:`chfl_topology%remove`
    :field subroutine add_bond: :f:func:`chfl_topology%add_bond`
    :field subroutine remove_bond: :f:func:`chfl_topology%remove_bond`
    :field subroutine isbond: :f:func:`chfl_topology%isbond`
    :field subroutine isangle: :f:func:`chfl_topology%isangle`
    :field subroutine isdihedral: :f:func:`chfl_topology%isdihedral`
    :field subroutine bonds_count: :f:func:`chfl_topology%bonds_count`
    :field subroutine angles_count: :f:func:`chfl_topology%angles_count`
    :field subroutine dihedrals_count: :f:func:`chfl_topology%dihedrals_count`
    :field subroutine bonds: :f:func:`chfl_topology%bonds`
    :field subroutine angles: :f:func:`chfl_topology%angles`
    :field subroutine dihedrals: :f:func:`chfl_topology%dihedrals`
    :field subroutine residues_count: :f:func:`chfl_topology%residues_count`
    :field subroutine add_residue: :f:func:`chfl_topology%add_residue`
    :field subroutine residues_linked: :f:func:`chfl_topology%residues_linked`
    :field subroutine free: :f:func:`chfl_topology%free`

.. f:subroutine:: chfl_topology%init([status])

    Initialize this topology with a new empty topology.

    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%from_frame(frame, [status])

    Initialize this topology with a copy of the topology of ``frame``.

    :argument chfl_frame frame: the frame
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%copy(topology, [status])

    Initialize this topology with a copy of ``topology``.

    :argument chfl_topology topology: topology to copy
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%atoms_count(natoms, [status])

    Get the number of atoms in the topology in ``natoms``.

    :argument integer natoms: number of atoms in the topology
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%resize(natoms, [status])

    Resize the topology to hold ``natoms`` atoms. If the new number of atoms is
    bigger than the current number, new atoms will be created with an empty name
    and type. If it is lower than the current number of atoms, the last atoms
    will be removed, together with the associated bonds, angles and dihedrals.

    :argument integer natoms: new size of the topology
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%add_atom(atom, [status])

    Add a copy of ``atom`` at the end of the topology.

    :argument chfl_atom atom: atom to be added
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.


.. f:subroutine:: chfl_topology%remove(i, [status])

    Remove the atom at index ``i`` from the topology.

    This shifts all the atoms indexes after ``i`` by 1 (n becomes n-1).

    :argument integer i: index of the atom to remove
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%isbond(i, j, result, [status])

    Check if the atoms at indexes ``i`` and ``j`` are bonded together, and store
    the result in ``result``.

    :argument integer i: atomic index of the first atom
    :argument integer j: atomic index of the second atom
    :argument logical result [kind=1]: ``.true.`` if the atoms are bonded,
        ``.false.`` otherwise
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%isangle(i, j, k, result, [status])

    Check if the atoms at indexes ``i``, ``j`` and ``k`` form an angle, and
    store the result in ``result``.

    :argument integer i: atomic index of the first atom
    :argument integer j: atomic index of the second atom
    :argument integer k: atomic index of the third atom
    :argument logical result [kind=1]: ``.true.`` if the atoms form an angle,
        ``.false.`` otherwise
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%isdihedral(i, j, k, m, result, [status])

    Check if the atoms at indexes ``i``, ``j``, ``k`` and ``m`` form a dihedral
    angle, and store the result in ``result``.

    :argument integer i: atomic index of the first atom
    :argument integer j: atomic index of the second atom
    :argument integer k: atomic index of the third atom
    :argument integer m: atomic index of the fourth atom
    :argument logical result [kind=1]: ``.true.`` if the atoms form a dihedral
        angle, ``.false.`` otherwise
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%bonds_count(nbonds, [status])

    Get the number of bonds in the topology in ``nbonds``.

    :argument integer nbonds: number of bonds
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%angles_count(nangles, [status])

    Get the number of angles in the topology in ``nangles``.

    :argument integer nangles: number of angles
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%dihedrals_count(ndihedrals, [status])

    Get the number of dihedral angles in the topology in ``ndihedrals``.

    :argument integer ndihedrals: number of dihedral angles
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%bonds(data, nbonds, [status])

    Get the list of bonds in the topology in the pre-allocated array ``data``
    of size ``2 x nbonds``.

    ``data`` size must be passed in the ``nbonds`` parameter, and be equal to
    the result of :f:func:`chfl_topology%bonds_count`.

    :argument integer data(2, nbonds): ``2 x nbonds`` array to be filled with
        the bonds in the system
    :argument integer nbonds: size of the array. This should be equal to the
        value given by :f:func:`chfl_topology%bonds_count`.
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%angles(data, nangles, [status])

    Get the list of angles in the ``topology`` in the pre-allocated array
    ``data`` of size ``3 x nangles``.

    ``data`` size must be passed in the ``nangles`` parameter, and be equal to the
    result of :f:func:`chfl_topology%angles_count`.

    :argument integer data(3, nangles): ``3 x nangles`` array to be filled with
        the angles in the system
    :argument integer nangles: size of the array. This should be equal to the
        value given by :f:func:`chfl_topology%angles_count`.
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%dihedrals(data, ndihedrals, [status])

    Get the list of dihedral angles in the topology in the pre-allocated array
    ``data`` of size ``4 x ndihedrals``.

    ``data`` size must be passed in the ``ndihedrals`` parameter, and be equal
    to the result of :f:func:`chfl_topology%dihedrals_count`.

    :argument integer data(4, ndihedrals): ``4 x ndihedrals`` array to be
        filled with the dihedral angles in the system
    :argument integer ndihedrals: size of the array. This should be equal to
        the value given by :f:func:`chfl_topology%dihedrals_count`.
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%add_bond(i, j, [status])

    Add a bond between the atoms at indexes ``i`` and ``j`` in the topology

    :argument integer i: atomic index of the first atom of the bond
    :argument integer j: atomic index of the second atom of the bond
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%remove_bond(i, j, [status])

    Remove any existing bond between the atoms at indexes ``i`` and ``j`` in the
    topology.

    This function does nothing if there is no bond between ``i`` and ``j``.

    :argument integer i: The atomic index of the first atom
    :argument integer j: The atomic index of the second atom
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%residues_count(natoms, [status])

    Get the number of residues in the topology in ``nresidues``.

    :argument integer natoms: number of residues
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%add_residue(residue, [status])

    Add a copy of ``residue`` to this topology.

    The residue id must not already be in the topology, and the residue must
    contain only atoms that are not already in another residue.

    :argument chfl_residue residue: residue to add in the topology
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%residues_linked(first, second, are_linked, [status])

    Check if the two residues ``first`` and ``second`` from the topology are
    linked together, *i.e.* if there is a bond between one atom in the first
    residue and one atom in the second one, and store the result in ``result``.

    :argument chfl_residue first: first residue
    :argument chfl_residue second: second residue
    :argument logical are_linked [kind=1]: ``.true.`` if the residues are
        linked, ``.false.`` otherwise
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_topology%free([status])

    Destroy a topology, and free the associated memory

    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.
