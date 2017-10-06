
``chfl_frame`` type
-------------------

.. f:type:: chfl_frame

    A :f:type:`chfl_frame` contains data from one simulation step: the current unit
    cell, the topology, the positions, and the velocities of the particles in
    the system. If some information is missing (topology or velocity or unit
    cell), the corresponding data is filled with a default value.

    The initialization routine for :f:type:`chfl_frame` are
    :f:func:`chfl_frame%init` and :f:func:`chfl_frame%copy`.

    :field subroutine init: :f:func:`chfl_frame%init`
    :field subroutine copy: :f:func:`chfl_frame%copy`
    :field subroutine atoms_count: :f:func:`chfl_frame%atoms_count`
    :field subroutine add_atom: :f:func:`chfl_frame%add_atom`
    :field subroutine remove: :f:func:`chfl_frame%remove`
    :field subroutine resize: :f:func:`chfl_frame%resize`
    :field subroutine positions: :f:func:`chfl_frame%positions`
    :field subroutine velocities: :f:func:`chfl_frame%velocities`
    :field subroutine add_velocities: :f:func:`chfl_frame%add_velocities`
    :field subroutine has_velocities: :f:func:`chfl_frame%has_velocities`
    :field subroutine set_cell: :f:func:`chfl_frame%set_cell`
    :field subroutine set_topology: :f:func:`chfl_frame%set_topology`
    :field subroutine guess_topology: :f:func:`chfl_frame%guess_topology`
    :field subroutine step: :f:func:`chfl_frame%step`
    :field subroutine set_step: :f:func:`chfl_frame%set_step`
    :field subroutine free: :f:func:`chfl_frame%free`

.. f:subroutine:: chfl_frame%init([status])

    Initialize this unit cell with a new empty frame. It will be resized by the
    library as needed.

    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_frame%copy(frame, [status])

    Initialize this frame with a copy of ``frame``.

    :argument chfl_frame frame: frame to copy
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_frame%atoms_count(natoms, [status])

    Get the current number of atoms in the frame in ``natoms``.

    :argument integer natoms: number of atoms in the frame
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_frame%add_atom(atom, position, [velocity, status])

    Add a :f:type:`chfl_atom` and the corresponding ``position`` and
    ``velocity`` data to this frame. ``velocity`` can be absent if no velocity
    is associated with this frame.

    :argument chfl_atom atom: atom to add to the frame
    :argument real position(3): atom position
    :optional real velocity(3) [optional]: atom velocity
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_frame%remove(index, [status])

    Remove the atom at the given ``index`` in the frame.

    This modify all the atoms indexes after ``index``, and invalidate any
    pointer obtained using :f:func:`chfl_frame%positions` or
    :f:func:`chfl_frame%velocities`.

    :argument integer index: index of the atom to remove
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_frame%resize(natoms, [status])

    Resize the positions, velocities and topology in the frame, to have space
    for ``natoms`` atoms.

    This function may invalidate any pointer to the positions or the velocities
    if the new size is bigger than the old one. In all the cases, previous data
    is conserved. This function conserve the presence or absence of velocities.

    :argument integer natoms: the new number of atoms in the frame
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_frame%positions(data, size, [status])

    Get a pointer to the positions array from the frame.

    This function set the ``data`` array to be the internal positions array.
    This array is a ``natoms x 3`` array, and the number of atoms will be in the
    ``size`` parameter.

    This function gives access to chemfiles internal data structure, and do not
    perform any copy, both when reading and writing the positions.

    If the frame is resized (by writing to it, or calling
    :f:func:`chfl_frame%resize`), the pointer is invalidated. If the frame is
    freed using :f:func:`chfl_frame%free`, the pointer is freed too.

    :argument real data(\:, \:) [pointer]: pointer to a float array containing
        the positions
    :argument integer size: number of atom, *i.e.* size of the ``data`` array
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_frame%velocities(data, size, [status])

    Get a pointer to the velocities array from the frame.

    This function set the ``data`` array to be the internal positions array.
    This array is a ``natoms x 3`` array, and the number of atoms will be in the
    ``size`` parameter.

    This function gives access to chemfiles internal data structure, and do not
    perform any copy, both when reading and writing the velocities.

    If the frame is resized (by writing to it, or calling
    :f:func:`chfl_frame%resize`), the pointer is invalidated. If the frame is
    freed using :f:func:`chfl_frame%free`, the pointer is freed too.

    :argument real data(\:, \:) [pointer]: pointer to a float array containing
        the velocities
    :argument integer size: number of atom, *i.e.* size of the ``data`` array
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_frame%add_velocities([status])

    Add velocity data to this frame.

    The velocities ar initialized to zero. If the frame already has velocities,
    this does nothing.

    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_frame%has_velocities(result, [status])

    Check if this frame contains velocity data.

    :argument logical result [kind=1]: ``.true.`` if the frame has velocities,
        ``.false.`` otherwise.
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_frame%set_cell(cell, [status])

    Set the :f:type:`chfl_cell` of this frame to ``cell``.

    :argument chfl_cell cell: new unit cell of the frame
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_frame%set_topology(topology, [status])

    Set the :f:type:`chfl_topology` of this frame to ``topology``.

    Calling this function with a topology that does not contain the right number
    of atom will return an error.

    :argument chfl_topology topology: new topology of the frame
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_frame%step(step, [status])

    Get the frame step, *i.e.* the frame number in the trajectory in ``step``.

    :argument integer step: frame step number
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_frame%set_step(step, [status])

    Set the frame step, *i.e.* the frame number in the trajectory to ``step``.

    :argument integer step: The new frame step
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_frame%guess_topology([status])

    Guess the bonds, angles and dihedrals in the frame.

    The bonds are guessed using a distance-based algorithm, and then angles and
    dihedrals are guessed from the bonds.

    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_frame%free([status])

    Destroy a frame, and free the associated memory

    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.
