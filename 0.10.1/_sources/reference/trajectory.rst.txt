``chfl_trajectory`` type
------------------------

.. f:type:: chfl_trajectory

    The :f:type:`chfl_trajectory` type is the main entry point when using
    chemfiles. A :f:type:`chfl_trajectory` behave a like a file, allowing to
    read and/or write :f:type:`chfl_frame`.

    :field subroutine open: :f:func:`chfl_trajectory%open`
    :field subroutine memory_reader: :f:func:`chfl_trajectory%memory_reader`
    :field subroutine memory_writer: :f:func:`chfl_trajectory%memory_writer`
    :field subroutine memory_buffer: :f:func:`chfl_trajectory%memory_buffer`
    :field function path: :f:func:`chfl_trajectory%path`
    :field function nsteps: :f:func:`chfl_trajectory%nsteps`
    :field subroutine read: :f:func:`chfl_trajectory%read`
    :field subroutine read_step: :f:func:`chfl_trajectory%read_step`
    :field subroutine write: :f:func:`chfl_trajectory%write`
    :field subroutine set_topology: :f:func:`chfl_trajectory%set_topology`
    :field subroutine topology_file: :f:func:`chfl_trajectory%topology_file`
    :field subroutine set_cell: :f:func:`chfl_trajectory%set_cell`
    :field subroutine close: :f:func:`chfl_trajectory%close`

.. f:subroutine:: chfl_trajectory%open(path, mode, [format, status])

    Open the file at the given ``path`` using the given ``mode``.
    Valid modes are ``'r'`` for read, ``'w'`` for write and ``'a'`` for append.

    If ``format`` is not given or an empty string, the format will be guessed
    from the extension.

    This subroutine allocate memory which must be released with
    :f:func:`chfl_trajectory%close`.

    :argument character(len=\*) path: path to the trajectory file
    :argument character mode: opening mode
    :optional character(len=\*) format: format to use to read and write to the file
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_trajectory%memory_reader(data, format, [status])

    Read the ``data`` memory buffer as though it were a formatted file.
    The ``format`` parameter is required and may contain a compression method.

    This subroutine allocate memory which must be released with
    :f:func:`chfl_trajectory%close`.

    :argument character(len=\*) data: memory to read
    :argument character(len=\*) format: format uses to read the buffer
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_trajectory%memory_writer(format, [status])

    Write to a memory buffer as though it were a formatted file

    The ``format`` parameter is required. To retrieve the memory written to by
    the :f:type:`chfl_trajectory`, use :f:type:`chfl_trajectory%memory_buffer`.

    This subroutine allocate memory which must be released with
    :f:func:`chfl_trajectory%close`.

    :argument character(len=\*) format: format used to write to the file
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_trajectory%memory_buffer(buffer, [status])

    Obtain the memory buffer written to by the :f:type:`chfl_trajectory`.

    :argument character(len=:) buffer [allocatable]: buffer to fill with the
        data written to this trajectory
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_trajectory%path([status])

    Get the path used to open the trajectory. If the path is longer than
    :f:var:`CHFL_STRING_LENGTH`, it will be truncated.

    :return character(len=CHFL_STRING_LENGTH):
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_trajectory%nsteps([status])

    Get the number of steps (the number of frames) in the trajectory.

    :return integer:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_trajectory%read(frame, [status])

    Read the next step of the trajectory into a ``frame``.

    If the number of atoms in frame does not correspond to the number of atom in
    the next step, the frame is resized.

    :argument type(chfl_frame) frame: frame to fill with the data
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_trajectory%read_step(step, frame, [status])

    Read a specific ``step`` of the trajectory into a ``frame``. The first
    trajectory step is the step 0.

    If the number of atoms in frame does not correspond to the number of atom
    in the step, the frame is resized.

    :argument integer step: step to read
    :argument type(chfl_frame) frame: frame to fill with the data
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_trajectory%write(frame, [status])

    Write a single ``frame`` to the trajectory.

    :argument type(chfl_frame) frame: frame to be writen to the file
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_trajectory%set_topology(topology, [status])

    Set the ``topology`` associated with the trajectory. This topology will be
    used when reading and writing the files, replacing any topology in the
    frames or files.

    :argument type(chfl_topology) topology: new topology to use
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_trajectory%topology_file(path, [format, status])

    Set the topology associated with the trajectory by reading the first frame of
    the file at the given ``path`` using the file format in ``format``; and
    extracting the topology of this frame.

    If ``format`` is an empty string or not given, the format will be guessed
    from the extension.

    :argument character(len=\*) path: file to read in order to get the new topology
    :optional character(len=\*) format: format to use for the topology file
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_trajectory%set_cell(cell, [status])

    Set the unit ``cell`` associated with the trajectory. This cell will be used
    when reading and writing the files, replacing any pre-existing unit cell.

    :argument type(chfl_cell) cell: new cell to use
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.


.. f:subroutine:: chfl_trajectory%close([status])

    Close a trajectory file, and free the associated memory.

    Closing a file will synchronize all changes made to the file with the
    storage (hard drive, network, ...) used for this file.
