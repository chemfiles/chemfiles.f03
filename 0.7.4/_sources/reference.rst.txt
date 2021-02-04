.. _fortran-api:

Fortran interface reference
===========================

The ``chemfiles`` module is built around the main types of chemfiles:
:f:type:`chfl_trajectory`, :f:type:`chfl_frame`, :f:type:`chfl_cell`,
:f:type:`chfl_topology`, :f:type:`chfl_residue`, :f:type:`chfl_atom` and
:f:type:`chfl_selection`. For more information about these types, please see the
chemfiles `overview`_.

.. warning::

    Atomic indexes in chemfiles starts at 0, not 1. That means that the first
    atom in a frame have the index 0, not 1. This should be taken in account
    when using chemfiles functions. Fortran arrays returned by function still
    have indexes starting at 1.

    .. code-block :: fortran

        program indexing
            use iso_fortran_env, only: real64, int64
            use chemfiles
            implicit none
            type(chfl_frame)      :: frame
            type(chfl_atom)       :: atom
            real(real64), pointer :: positions(:, :)
            integer(int64)        :: natoms

            ! Initialize the frame ...

            ! Get the first atom in the frame
            call atom%from_frame(frame, 0)
            ! Get the second atom in the frame
            call atom%from_frame(frame, 1)

            call frame%poositions(positions, natoms)
            ! position(1, :) now contains the positions of the first atom
        end program

.. _overview: http://chemfiles.org/chemfiles/latest/overview.html

Conventions
-----------

All the functions and types have the ``chfl_`` prefix. All the functions take an
optional ``status`` argument which will indicate the status of the operation. It
should be ``CHFL_SUCCESS`` if everything was OK, and another value indicating in
case of error. The only exeption to this rule are the functions returnning
character strings: ``chfl_version`` and ``chfl_last_error()``.

When creating a variable of one of the chemfiles types, the first routine to be
called should be an initialization routine. It can be either the ``init``
routine for default initialization, or another routine documented as
initializing.

.. code-block:: fortran

    type(chfl_cell) :: cell
    type(chfl_frame) :: frame

    ! Initialize the variables
    call cell%init([20, 20, 20])
    call frame%init()

    ! free the memory
    call cell%free()
    call frame%free()

These initialization function should only be called once. In order to free the
memory asssociated with any chemfiles variable, the ``free`` subroutine should
be called. After a call the the ``free`` subroutine, the ``init`` subroutine can
be called again whithout any memory leak risk. Not initializing chemfiles
variables will lead to errors.

Error and logging functions
---------------------------

.. f:function:: chfl_version()

    Get the version of the Chemfiles library.

    :return character [len=*]: chemfiles version

.. f:function:: chfl_last_error()

    Get the last error message emmited by Chemfiles.

    :return character [len=*]: error message for the last error

.. f:subroutine:: chfl_clear_errors([status])

    Clear the last error message emmited by Chemfiles.

    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_set_warning_callback(callback, [status])

    Chemfiles sends warning on various events, for example invalid files or
    errors in the API usage. By default they are printed to the standard error
    stream, but you can redirect them by setting a callback to be called on each
    event with the event message. This function set the callback for all warning
    events.

    :parameter procedure callback [kind=chfl_warning_callback]: warning callback
    :optional integer status [optional, kind=chfl_status]: status code of the
          operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
          about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_warning_callback(message)

    Interface for the warning callback to be used with
    ``chfl_set_warning_callback``.

    :parameter character message [len=*, intent(in)]: The warning message


The optional status argument is an integer of kind ``chfl_status``, which can
take the following values:

.. f:variable:: integer(chfl_status) :: CHFL_SUCCESS

    Status for successful operations.

.. f:variable:: integer(chfl_status) :: CHFL_MEMORY_ERROR

    Status code for error concerning memory: out of memory, wrong size for
    pre-allocated buffers, *etc.*

.. f:variable:: integer(chfl_status) :: CHFL_FILE_ERROR

    Status code for error concerning files: the file do not exist, the user
    does not have rights to open it, *etc.*

.. f:variable:: integer(chfl_status) :: CHFL_FORMAT_ERROR

    Status code for error in file formating, i.e. for invalid files.

.. f:variable:: integer(chfl_status) :: CHFL_SELECTION_ERROR

    Status code for invalid selection strings.

.. f:variable:: integer(chfl_status) :: CHFL_GENERIC_ERROR

    Status code for any other error from Chemfiles.

.. f:variable:: integer(chfl_status) :: CHFL_CXX_ERROR

    Status code for error in the C++ standard library.

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

``chfl_cell`` type
------------------

.. f:currentmodule:: chfl_cell

.. f:type:: chfl_cell

    A :f:type:`chfl_cell` represent the box containing the atoms, and its
    periodicity.

    An unit cell is fully represented by three lengths (a, b, c); and three
    angles (alpha, beta, gamma). The angles are stored in degrees, and the
    lengths in Angstroms.

    The initialization routine for :f:type:`chfl_cell` are
    :f:func:`chfl_cell%init`, :f:func:`chfl_cell%triclinic`,
    :f:func:`chfl_cell%from_frame` and :f:func:`chfl_cell%copy`.

    :field subroutine init: :f:func:`chfl_cell%init`
    :field subroutine triclinic: :f:func:`chfl_cell%triclinic`
    :field subroutine from_frame: :f:func:`chfl_cell%from_frame`
    :field subroutine copy: :f:func:`chfl_cell%copy`
    :field subroutine lengths: :f:func:`chfl_cell%lengths`
    :field subroutine set_lengths: :f:func:`chfl_cell%set_lengths`
    :field subroutine angles: :f:func:`chfl_cell%angles`
    :field subroutine set_angles: :f:func:`chfl_cell%set_angles`
    :field subroutine matrix: :f:func:`chfl_cell%matrix`
    :field subroutine shape: :f:func:`chfl_cell%shape`
    :field subroutine set_shape: :f:func:`chfl_cell%set_shape`
    :field subroutine volume: :f:func:`chfl_cell%volume`
    :field subroutine free: :f:func:`chfl_cell%free`


.. f:subroutine:: chfl_cell%init(lengths, [status])

    Initialize this unit cell with an unit cell having the given ``lengths``.
    The unit cell shape is :f:var:`CHFL_CELL_ORTHORHOMBIC`.

    :argument real lengths(3): cell lengths, in angstroms
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%triclinic(lengths, angles, [status])

    Initialize this unit cell with an unit cell having the given ``lengths`` and
    ``angles``. The unit cell shape is :f:var:`CHFL_CELL_TRICLINIC`.

    :argument real lengths(3): cell lengths, in angstroms
    :argument real angles(3): cell angles, in degrees
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%copy(cell, [status])

    Initialize this unit cell with a copy of ``cell``.

    :argument chfl_cell cell: cell to copy
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%from_frame(frame, [status])

    Initialize this topology with a copy of the :f:type:`chfl_cell` of a frame.

    :argument chfl_frame frame: the frame
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%volume(volume, [status])

    Get the volume of the unit cell in ``volume``.

    :argument real volume: volume of the unit cell
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%lengths(lengths, [status])

    Get the unit cell lengths in ``lengths``.

    :argument real lengths(3): cell lengths, in angstroms
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%set_lengths(lengths, [status])

    Set the unit cell lengths to ``lengths``.

    :argument real lengths(3): new cell lengths, in angstroms
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%angles(angles, [status])

    Get the unit cell angles in ``angles``.

    :argument real angles(3): cell angles, in degrees
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%set_angles(alpha, beta, gamma, [status])

    Set the cell angles to ``angles``. Trying to set cell angles on a cell which
    is not triclinic (does not have the ``CHFL_CELL_TRICLINIC`` shape) is an
    error.

    :argument real angles(3): new cell angles, in degrees
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%matrix(matrix, [status])

    Get the unit cell matricial representation in ``matrix``.

    The unit cell representation is obtained by aligning the a vector along the
    *x* axis and putting the b vector in the *xy* plane. This make the matrix
    an upper triangular matrix:

    .. code-block:: sh

        | a_x b_x c_x |
        |  0  b_y c_y |
        |  0   0  c_z |


    :argument real matrix(3, 3): unit cell matrix
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%shape(shape, [status])

    Get the unit cell shape in ``shape``.

    :argument integer type [kind=chfl_cell_shape_t]: the shape of the cell
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

    The cell shapes are integers which ``kind`` is the ``chfl_cell_shape_t``
    parameter:

    .. f:variable:: integer(chfl_cell_shape_t) :: CHFL_CELL_ORTHORHOMBIC

        The three angles are 90°

    .. f:variable:: integer(chfl_cell_shape_t) :: CHFL_CELL_TRICLINIC

        The three angles may not be 90°

    .. f:variable:: integer(chfl_cell_shape_t) :: CHFL_CELL_INFINITE

        Cell type when there is no periodic boundary conditions

.. f:subroutine:: chfl_cell%set_shape(shape, [status])

    Set the unit cell shape to ``shape``

    :argument integer type [kind=chfl_cell_shape_t]: the new type of the cell
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%free([status])

    Destroy an unit cell, and free the associated memory

    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

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

``chfl_residue`` type
----------------------

.. f:type:: chfl_residue

    A :f:type:`chfl_residue` is a group of atoms belonging to the same logical
    unit. They can be small molecules, amino-acids in a protein, monomers in
    polymers, *etc.*

    The initialization routine for :f:type:`chfl_residue` are
    :f:func:`chfl_residue%init`, :f:func:`chfl_residue%from_topology`,
    :f:func:`chfl_residue%for_atom` and :f:func:`chfl_residue%copy`.

    :field subroutine init: :f:func:`chfl_residue%init`
    :field subroutine copy: :f:func:`chfl_residue%copy`
    :field subroutine from_topology: :f:func:`chfl_residue%from_topology`
    :field subroutine for_atom: :f:func:`chfl_residue%for_atom`
    :field subroutine name: :f:func:`chfl_residue%name`
    :field subroutine id: :f:func:`chfl_residue%id`
    :field subroutine atoms_count: :f:func:`chfl_residue%atoms_count`
    :field subroutine add_atom: :f:func:`chfl_residue%add_atom`
    :field subroutine contains: :f:func:`chfl_residue%contains`
    :field subroutine free: :f:func:`chfl_residue%free`

.. f:subroutine:: chfl_residue%init(name, [id, status])

    Initialize the residue with a new residue with the given ``name`` and
    optional residue identifier ``id``.

    :argument character name [len=*]: residue name
    :optional integer id: residue id
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

    :argument character name [len=buffsize]: string buffer to be filled with
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

.. f:subroutine:: chfl_residue%free([status])

    Destroy a residue, and free the associated memory

    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

``chfl_atom`` type
------------------

.. f:type:: chfl_atom

    A :f:type:`chfl_atom` is a particle in the current :f:type:`chfl_frame`. It stores the
    following atomic properties:

    - atom name;
    - atom type;
    - atom mass;
    - atom charge.

    The atom name is usually an unique identifier (``H1``, ``C_a``) while the
    atom type will be shared between all particles of the same type: ``H``,
    ``Ow``, ``CH3``.

    The initialization routine for :f:type:`chfl_atom` are
    :f:func:`chfl_atom%init`, :f:func:`chfl_atom%from_frame` and
    :f:func:`chfl_atom%from_topology`.

    :field subroutine init: :f:func:`chfl_atom%init`
    :field subroutine copy: :f:func:`chfl_atom%copy`
    :field subroutine from_frame: :f:func:`chfl_atom%from_frame`
    :field subroutine from_topology: :f:func:`chfl_atom%from_topology`
    :field subroutine mass: :f:func:`chfl_atom%mass`
    :field subroutine set_mass: :f:func:`chfl_atom%set_mass`
    :field subroutine charge: :f:func:`chfl_atom%charge`
    :field subroutine set_charge: :f:func:`chfl_atom%set_charge`
    :field subroutine type: :f:func:`chfl_atom%type`
    :field subroutine set_type: :f:func:`chfl_atom%set_type`
    :field subroutine name: :f:func:`chfl_atom%name`
    :field subroutine set_name: :f:func:`chfl_atom%set_name`
    :field subroutine full_name: :f:func:`chfl_atom%full_name`
    :field subroutine vdw_radius: :f:func:`chfl_atom%vdw_radius`
    :field subroutine covalent_radius: :f:func:`chfl_atom%covalent_radius`
    :field subroutine atomic_number: :f:func:`chfl_atom%atomic_number`
    :field subroutine free: :f:func:`chfl_atom%free`

.. f:subroutine:: chfl_atom%init(name, [status])

    Initialize this atom with the given ``name``, and set the atom type to
    ``name``.

    :argument character name [len=*]: atom name
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_atom%copy(atom, [status])

    Initialize this atom with a copy of ``atom``.

    :argument chfl_atom atom: atom to copy
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_atom%from_frame(frame, i, [status])

    Initialize this atom with a copy the atom at index ``i`` from a ``frame``.

    :argument chfl_frame frame: frame
    :argument integer i: atom index in the frame
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_atom%from_topology(topology, i, [status])

    Initialize this atom with a copy the atom at index ``i`` from a
    ``topology``.

    :argument chfl_topology topology: topology
    :argument integer idx: atom index in the topology
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_atom%mass(mass, [status])

    Get the mass of tah atom in ``mass``. The mass is in atomic mass units.

    :argument real mass: atom mass
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_atom%set_mass(mass, [status])

    Set the mass of the atom to ``mass``. The mass should be in atomic mass
    units.

    :argument real mass: new atom mass
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_atom%charge(charge, [status])

    Get the charge of the atom in ``charge``. The charge is in number of the
    electron charge *e*.

    :argument real charge: The atom charge
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_atom%set_charge(charge, [status])

    Get the charge of the atom to ``charge``. The charge should be in number of
    the electron charge *e*.

    :argument real charge: new atom charge
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_atom%name(name, buffsize, [status])

    Get the name of an atom in the string buffer ``name``.

    The buffer size must be passed in ``buffsize``. This function will truncate
    the name to fit in the buffer.

    :argument character name [len=buffsize]: string buffer to be filled with
        the atom name
    :argument buffsize: length of the string buffer
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_atom%set_name(name, [status])

    Set the name of an atom to ``name``.

    :argument character name [len=*]: new atom name
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_atom%full_name(name, buffsize, [status])

    Get the full name of an ``atom`` from its type in the string buffer
    ``name``.

    The buffer size must be passed in ``buffsize``. This function will truncate
    the name to fit in the buffer.

    :argument character name [len=buffsize]: string buffer to be filled with
        the atom full name
    :argument buffsize: length of the string buffer
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_atom%type(type, buffsize, [status])

    Get the type of an atom in the string buffer ``type``.

    The buffer size must be passed in ``buffsize``. This function will truncate
    the type to fit in the buffer.

    :argument character name [len=buffsize]: string buffer to be filled with
        the atom type
    :argument buffsize: length of the string buffer
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_atom%set_type(type, [status])

    Set the type of an atom to ``type``.

    :argument character name [len=*]: new atom type
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_atom%vdw_radius(radius, [status])

    Get the Van der Waals radius of an atom from the atom type in ``radius``.

    If the radius in unknown, this function set ``radius`` to -1.

    :argument real radius: Van der Waals radius
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_atom%covalent_radius(radius, [status])

    Get the covalent radius of an atom from the atom type in ``radius``.

    If the radius in unknown, this function set ``radius`` to -1.

    :argument real radius: covalent radius
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_atom%atomic_number(number, [status])

    Get the atomic number of an atom from the atom type in ``number``.

    If the atomic number in unknown, this function set ``number`` to -1.

    :argument integer number: atomic number
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_atom%free([status])

    Destroy an atom, and free the associated memory

    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.


``chfl_selection`` type
------------------------

.. f:type:: chfl_selection

    :f:type:`chfl_selection` allow to select atoms in a :f:type:`chfl_frame`,
    from a selection language. The selection language is built by combining
    basic operations. Each basic operation follows the
    ``<selector>[(<variable>)] <operator> <value>`` structure, where
    ``<operator>`` is a comparison operator in ``== != < <= > >=``.

    The initialization routines for :f:type:`chfl_selection` are
    :f:func:`chfl_selection%init` and :f:func:`chfl_selection%copy`.

    :field subroutine init: :f:func:`chfl_selection%init`
    :field subroutine copy: :f:func:`chfl_selection%copy`
    :field subroutine size: :f:func:`chfl_selection%size`
    :field subroutine string: :f:func:`chfl_selection%string`
    :field subroutine evaluate: :f:func:`chfl_selection%evaluate`
    :field subroutine matches: :f:func:`chfl_selection%matches`
    :field subroutine free: :f:func:`chfl_selection%free`

.. f:subroutine:: chfl_selection%init(selection, [status])

    Initialize the selection with a new selection from the given ``selection``
    string.

    See the `selection documentation`_ for the selection language specification.

    .. _selection documentation: http://chemfiles.org/chemfiles/latest/selections.html

    :argument character selection [len=*]: The selection string
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_selection%copy(selection, [status])

    Initialize the selection with a copy of ``selection``.

    The copy does not contains any state, and :f:func:`chfl_selection%evaluate`
    must be called again before using :f:func:`chfl_selection%matches`.

    :argument chfl_selection selection: selection to copy
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_selection%size(size, [status])

    Get the size of the selection in ``size``.

    The size of a selection is the number of atoms we are selecting together.
    This value is 1 for the 'atom' context, 2 for the 'pair' and 'bond' context,
    3 for the 'three' and 'angles' contextes and 4 for the 'four' and 'dihedral'
    contextes.

    :argument integer size: selection size
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_selection%string(string, buffsize, [status])

    Get the selection string used to create this selection in the ``string``
    buffer.

    The buffer size must be passed in ``buffsize``. This function will truncate
    the selection string to fit in the buffer.

    :argument character size [len=buffsize]: string buffer to be filled with the
        initial selection string
    :argument integer buffsize: size of the string buffer
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.


.. f:subroutine:: chfl_selection%evaluate(frame, nmatches, [status])

    Evaluate the selection for a given ``frame``, and store the number of
    matches in ``nmatches``. Use :f:func:`chfl_selection%matches` to get the
    matches.

    :argument chfl_frame frame: frame to evaluate
    :argument integer n_matches: number of matches for this selection
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.


.. f:subroutine:: chfl_selection%matches(matches, n, [status])

    Get the matches for the ``selection`` after a call to
    :f:func:`chfl_selection%evalutate`, in the pre-allocated ``matches`` array.
    The size of the ``matches`` array must be passed in ``n``.

    :argument chfl_match matches(nmatchs) [allocatable]: Pre-allocated array of
        the size given by ``chfl_selection%evaluate``.
    :argument integer n: size of the ``matches`` array
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_selection%free([status])

    Destroy a selection, and free the associated memory

    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:type:: chfl_match

    This type contains the matched atoms for a given selection in the ``atoms``
    array. Values in the ``atoms`` array are valid up to the ``size`` of this
    match. If the match size is 2, then ``atom(1)`` and ``atom(2)`` are valid,
    and ``atom(3)`` and ``atom(4)`` contains invalid indexes.

    :field integer size: The size of this match.
    :field integer atoms(4): The index of the matched atoms.
