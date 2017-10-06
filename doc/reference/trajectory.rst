``chfl_trajectory`` type
------------------------

.. f:type:: chfl_trajectory

    The :f:type:`chfl_trajectory` type is the main entry point when using
    chemfiles. A :f:type:`chfl_trajectory` behave a like a file, allowing to
    read and/or write :f:type:`chfl_frame`.

    The initialization routine for :f:type:`chfl_trajectory` are
    :f:func:`chfl_trajectory%open` and :f:func:`chfl_trajectory%with_format`.
    The memory liberation routine is :f:func:`chfl_trajectory%close`.

    :field subroutine open: :f:func:`chfl_trajectory%open`
    :field subroutine with_format: :f:func:`chfl_trajectory%with_format`
    :field subroutine nsteps: :f:func:`chfl_trajectory%nsteps`
    :field subroutine read: :f:func:`chfl_trajectory%read`
    :field subroutine read_step: :f:func:`chfl_trajectory%read_step`
    :field subroutine write: :f:func:`chfl_trajectory%write`
    :field subroutine set_topology: :f:func:`chfl_trajectory%set_topology`
    :field subroutine topology_file: :f:func:`chfl_trajectory%topology_file`
    :field subroutine set_cell: :f:func:`chfl_trajectory%set_cell`
    :field subroutine close: :f:func:`chfl_trajectory%close`

.. f:subroutine:: chfl_trajectory%open(path, mode, , [status])

    Open the file at the given ``path`` using the given ``mode``.
    Valid modes are ``'r'`` for read, ``'w'`` for write and ``'a'`` for append.

    :argument character path [len=*]: path to the trajectory file
    :argument character mode: opening mode
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_trajectory%with_format(path, mode, format, [status])

    Open the trajectory at the given ``path`` using a specific file ``format``
    and the given ``mode``.

    This is be needed when the file format does not match the extension, or when
    there is not standard extension for this format. Valid modes are ``'r'`` for
    read, ``'w'`` for write and ``'a'`` for append.

    If ``format`` is an empty string, the format will be guessed from the
    extension.

    :argument character path [len=*]: path to the trajectory file
    :argument character mode: opening mode
    :argument character format [len=*]: format to use
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_trajectory%read(frame, [status])

    Read the next step of the trajectory into a ``frame``.

    If the number of atoms in frame does not correspond to the number of atom in
    the next step, the frame is resized.

    :argument chfl_frame frame: frame to fill with the data
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_trajectory%read_step(step, frame, [status])

    Read a specific ``step`` of the trajectory into a ``frame``. The first
    trajectory step is the step 0.

    If the number of atoms in frame does not correspond to the number of atom
    in the step, the frame is resized.

    :argument integer step: step to read
    :argument chfl_frame frame: frame to fill with the data
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_trajectory%write(frame, [status])

    Write a single ``frame`` to the trajectory.

    :argument chfl_frame frame: frame to be writen to the file
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_trajectory%set_topology(topology, [status])

    Set the ``topology`` associated with the trajectory. This topology will be
    used when reading and writing the files, replacing any topology in the
    frames or files.

    :argument chfl_topology topology: new topology to use
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_trajectory%topology_file(path, [format, status])

    Set the topology associated with the trajectory by reading the first frame of
    the file at the given ``path`` using the file format in ``format``; and
    extracting the topology of this frame.

    If ``format`` is an empty string or not given, the format will be guessed
    from the extension.

    :argument character path [len=*]: file to read in order to get the new topology
    :optional string format [optional]: format to use for the topology file
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_trajectory%set_cell(cell, [status])

    Set the unit ``cell`` associated with the trajectory. This cell will be used
    when reading and writing the files, replacing any pre-existing unit cell.

    :argument chfl_cell cell: new cell to use
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_trajectory%nsteps(nsteps, [status])

    Store the number of steps (the number of frames) from the trajectory in
    ``nsteps``.

    :argument integer nsteps: number of steps
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.


.. f:subroutine:: chfl_trajectory%close([status])

    Close a trajectory file, and free the associated memory.

    Closing a file will synchronize all changes made to the file with the
    storage (hard drive, network, ...) used for this file.

    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.
