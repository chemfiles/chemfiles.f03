``chfl_residue`` type
----------------------

.. f:type:: chfl_residue

    A :f:type:`chfl_residue` is a group of atoms belonging to the same logical
    unit. They can be small molecules, amino-acids in a protein, monomers in
    polymers, *etc.*

    The initialization routine for :f:type:`chfl_residue` are:

    - :f:func:`chfl_residue%init`;
    - :f:func:`chfl_residue%with_id`;
    - :f:func:`chfl_residue%from_topology`;
    - :f:func:`chfl_residue%for_atom`;
    - :f:func:`chfl_residue%copy`.

    :field subroutine init: :f:func:`chfl_residue%init`
    :field subroutine with_id: :f:func:`chfl_residue%with_id`
    :field subroutine copy: :f:func:`chfl_residue%copy`
    :field subroutine from_topology: :f:func:`chfl_residue%from_topology`
    :field subroutine for_atom: :f:func:`chfl_residue%for_atom`
    :field subroutine name: :f:func:`chfl_residue%name`
    :field subroutine id: :f:func:`chfl_residue%id`
    :field subroutine atoms_count: :f:func:`chfl_residue%atoms_count`
    :field subroutine add_atom: :f:func:`chfl_residue%add_atom`
    :field subroutine contains: :f:func:`chfl_residue%contains`
    :field subroutine atoms: :f:func:`chfl_residue%atoms`
    :field subroutine free: :f:func:`chfl_residue%free`

.. f:subroutine:: chfl_residue%init(name, [status])

    Initialize the residue with a new residue with the given ``name`` and no
    residue id.

    :argument character(len=*) name: residue name
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_residue%with_id(name, id, [status])

    Initialize the residue with a new residue with the given ``name`` and
    residue ``id``.

    :argument character(len=*) name: residue name
    :argument integer id: residue id
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_residue%copy(residue, [status])

    Initialize this residue with a copy of ``residue``.

    :argument chfl_residue residue: residue to copy
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_residue%from_topology(topology, i, [status])

    Initialize this residue with a copy of the residue at index ``i`` from a
    ``topology``. The residue index in the topology is not always the same as
    the residue id.

    :argument chfl_topology topology: topology
    :argument integer i: index of the residue in the topology
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_residue%for_atom(topology, i, [status])

    Get a copy of the residue containing the atom at index ``i`` in the
    ``topology``.

    :argument chfl_topology topology: topology
    :argument integer i: index of the atom in the topology
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_residue%name(name, buffsize, [status])

    Get the name of the residue in the string buffer ``name``.

    The buffer size must be passed in ``buffsize``. This function will truncate
    the residue name to fit in the buffer.

    :argument character(len=buffsize) name: string buffer to be filled with
        the residue name
    :argument buffsize: length of the string buffer
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_residue%id(id, [status])

    Get the identifier of the residue in the initial topology file in ``id``

    :argument integer id: identifier of the residue
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_residue%atoms_count(size, [status])

    Get the number of atoms in the residue in ``size``.

    :argument integer size: number of atoms in the residue
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_residue%add_atom(i, [status])

    Add the atom at index ``i`` in the residue.

    :argument integer i: index of the atom to add
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_residue%contains(i, result, [status])

    Check if the atom at index ``i`` is in the residue, and store the result in
    ``result``.

    :argument integer i: index of the atom
    :argument logical result [kind=1]: `.true.` if the atom is in the residue,
        `.false.` otherwise
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_residue%atoms(data, natoms, [status])

    Get the list of atoms in the topology in the pre-allocated array ``data``
    of size ``natoms``.

    ``data`` size must be passed in the ``natoms`` parameter, and be equal to
    the result of :f:func:`chfl_residue%atoms_count`.

    :argument integer data(natoms): ``natoms`` array to be filled with
        the atoms in the residue
    :argument integer natoms: size of the array. This should be equal to the
        value given by :f:func:`chfl_residue%atoms_count`.
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_residue%free([status])

    Destroy a residue, and free the associated memory

    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.
