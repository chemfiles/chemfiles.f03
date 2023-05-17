.. _fortran-api:

Fortran interface reference
===========================

The ``chemfiles`` module is built around the 5 main types of chemfiles: ``chfl_trajectory``,
``chfl_frame``, ``chfl_cell``, ``chfl_topology``, and ``chfl_atom``. For more
information about these types, please see the chemfiles `overview`_.

.. warning::

    Atomic indexes in chemfiles starts at 0, not 1. That means that the first atom in
    a frame have the index 0, not 1. This should be taken in account when using
    chemfiles function. Fortran arrays returned by function still have indexes
    starting at 1.

    .. code-block :: fortran

        program indexing
            use iso_fortran_env, only: real32, int64
            use chemfiles
            implicit none
            type(chfl_frame)      :: frame
            type(chfl_atom)       :: atom
            real(real32), pointer :: positions(:, :)
            integer(int64)        :: natoms

            ! Read the frame here

            ! Get the first atom in the frame
            call atom%from_frame(frame, 0)
            ! Get the second atom in the frame
            ! call atom%from_frame(frame, 1)

            call frame%poositions(positions, natoms)
            ! position(1, :) now contains the positions of the first atom
        end program

    This may change in future release to make all atomic number starting at 1, which
    is more familiar to Fortran developers and would be less confusing.

.. _overview: http://chemfiles.readthedocs.org/en/latest/overview.html

Naming conventions and call conventions
---------------------------------------

All the functions and types have the ``chfl_`` prefix. Except for the ``chfl_strerror``
and ``chfl_last_error`` functions, all the functions take a ``status`` argument,
which will indicate the status of the operation. It should be 0 if everything
was OK, and can be any other number in case of error.

When creating a variable of one of the chemfiles types, the first routine to be
called should be an initialization routine. It can be either the ``init`` routine
for default initialization, or another routine documented as initializing.

.. code-block:: fortran

    implicit none
    type(chfl_cell) :: cell
    type(chfl_frame) :: frame

    call cell%init(20, 20, 20, 90, 90, 90)
    call frame%init(3)

These initialization function should only be called once. In order to free the
memory asssociated with any chemfiles variable, the ``free`` subroutine should
be called. After a call the the ``free`` subroutine, the ``init`` subroutine
can be called again whithout any memory leak risk. Not initializing chemfiles
variables will lead to segmentations faults.

Miscellaneous functions
---------------------------

.. f:function:: string chfl_version([status])

    Get the version of the chemfiles library as a string.

    :optional integer status [optional]: The status code

Error and logging functions
---------------------------

.. f:function:: string chfl_strerror(status)

    Get the error message corresponding to an error code.

    :argument integer status: The status code
    :return string strerror: The error message corresponding to the status code

.. f:function:: string chfl_last_error()

    Get the last error message.

    :return string strerror: The error message corresponding to the status code

.. f:subroutine:: chfl_loglevel(level, [status])

    Get the current maximal logging level

    :argument integer level [kind=CHFL_LOG_LEVEL]: A variable that will contain the logging level
    :optional integer status [optional]: The status code

    The logging level are integers which ``kind`` is the parameter ``CHFL_LOG_LEVEL``:

    .. f:variable:: integer(CHFL_LOG_LEVEL) :: CHFL_LOG_ERROR

        Only log errors

    .. f:variable:: integer(CHFL_LOG_LEVEL) :: CHFL_LOG_WARNING

        Log warnings and erors. This is the default.

    .. f:variable:: integer(CHFL_LOG_LEVEL) :: CHFL_LOG_INFO

        Log infos, warnings and errors

    .. f:variable:: integer(CHFL_LOG_LEVEL) :: CHFL_LOG_DEBUG

        Log everything

.. f:subroutine:: chfl_set_loglevel(level, [status])

    Set the maximal logging level to ``level``

    :argument integer level [kind=CHFL_LOG_LEVEL]: The new logging level
    :optional integer status [optional]: The status code


.. f:subroutine:: chfl_logfile(file, [status])

    Redirect the logs to ``file``, overwriting the file if it exists.

    :argument string file: The path to the log file
    :optional integer status [optional]: The status code

.. f:subroutine:: chfl_log_stderr([status])

    Redirect the logs to the standard error output. This is enabled by default.

    :optional integer status [optional]: The status code

.. f:subroutine:: chfl_log_stdout([status])

    Redirect the logs to the standard output.

    :optional integer status [optional]: The status code

.. f:subroutine:: chfl_log_silent([status])

    Remove all logging output.

    :optional integer status [optional]: The status code

.. f:subroutine:: chfl_log_callback(callback, [status])

    Redirect all logging to user-provided logging. The ``callback`` subroutine will
    be called at each logging operation with the level of the message, and the the
    message itself.

    :parameter procedure(chfl_logging_callback) callback: The callback procedure
    :optional integer status [optional]: The status code

.. f:subroutine:: chfl_logging_callback(level, message)

    This is the interface for callback functions in the logging system. At every log
    event, this function will be called with the level and the message of the log
    event.

    :parameter integer level [intent(in)]: The level of the log event
    :parameter string message [intent(in)]: The message of the log event


``chfl_trajectory`` type
------------------------

.. f:currentmodule:: chfl_trajectory

.. f:type:: chfl_trajectory

    Wrapping around a C pointer of type ``CHFL_TRAJECTORY*``. The following
    subroutine are available:

    :field subroutine open:
    :field subroutine with_format:
    :field subroutine read:
    :field subroutine read_step:
    :field subroutine write:
    :field subroutine set_topology:
    :field subroutine set_topology_file:
    :field subroutine cell:
    :field subroutine nstep:
    :field subroutine sync:
    :field subroutine close:

    The initialization routine are ``open`` and ``with_format``, and the memory
    liberation routine is ``close``.

.. f:subroutine:: open(filename, mode, , [status])

    Open a trajectory file.

    :argument string filename: The path to the trajectory file
    :argument string mode: The opening mode: "r" for read, "w" for write and  "a" for append.
    :optional integer status [optional]: The status code

.. f:subroutine:: with_format(filename, mode, , [status])

    Open a trajectory file using a given format to read the file.

    :argument string filename: The path to the trajectory file
    :argument string mode: The opening mode: "r" for read, "w" for write and  "a" for append.
    :argument string format: The format to use
    :optional integer status [optional]: The status code

.. f:subroutine:: read(frame, [status])

    Read the next step of the trajectory into a frame

    :argument chfl_frame frame: A frame to fill with the data
    :optional integer status [optional]: The status code

.. f:subroutine:: read_step(step, frame, [status])

    Read a specific step of the trajectory in a frame

    :argument integer step: The step to read
    :argument chfl_frame frame: A frame to fill with the data
    :optional integer status [optional]: The status code

.. f:subroutine:: write(frame, [status])

    Write a frame to the trajectory.

    :argument chfl_frame frame: the frame which will be writen to the file
    :optional integer status [optional]: The status code

.. f:subroutine:: set_topology(topology, [status])

    Set the topology associated with a trajectory. This topology will be
    used when reading and writing the files, replacing any topology in the
    frames or files.

    :argument chfl_topology topology: The new topology to use
    :optional integer status [optional]: The status code

.. f:subroutine:: set_topology_file(filename, [status])

    Set the topology associated with a trajectory by reading the first
    frame of ``filename``; and extracting the topology of this frame.

    :argument string filename: The file to read in order to get the new topology
    :optional integer status [optional]: The status code

.. f:subroutine:: cell(cell, [status])

    Set the unit cell associated with a trajectory. This cell will be
    used when reading and writing the files, replacing any unit cell in the
    frames or files.

    :argument chfl_cell cell: The new cell to use
    :optional integer status [optional]: The status code

.. f:subroutine:: nsteps(nsteps, [status])

    Get the number of steps (the number of frames) in a trajectory.

    :argument integer nsteps: This will contain the number of steps
    :optional integer status [optional]: The status code

.. f:subroutine:: sync([status])

    Flush any buffered content to the hard drive.

    :optional integer status [optional]: The status code


.. f:subroutine:: close([status])

    Close a trajectory file, and free the associated memory

    :optional integer status [optional]: The status code

``chfl_frame`` type
-------------------

.. f:currentmodule:: chfl_frame

.. f:type:: chfl_frame

    Wrapping around a C pointer of type ``CHFL_FRAME*``. The following
    subroutine are available:

    :field subroutine init:
    :field subroutine atoms_count:
    :field subroutine resize:
    :field subroutine positions:
    :field subroutine velocities:
    :field subroutine add_velocities:
    :field subroutine has_velocities:
    :field subroutine set_cell:
    :field subroutine set_topology:
    :field subroutine step:
    :field subroutine set_step:
    :field subroutine selection:
    :field subroutine free:

.. f:subroutine:: init(natoms, [status])

    Create an empty frame with initial capacity of ``natoms``. It will be
    resized by the library as needed.

    :argument integer natoms: the size of the wanted frame
    :optional integer status [optional]: The status code

.. f:subroutine:: atoms_count(natoms, [status])

    Get the current number of atoms in the frame

    :argument integer natoms: the number of atoms in the frame
    :optional integer status [optional]: The status code

.. f:subroutine:: resize(natoms, [status])

    Resize the positions and the velocities in frame, to make space for ``natoms`` atoms.
    This function may invalidate any pointer to the positions or the velocities if the new
    size is bigger than the old one. In all the cases, previous data is conserved. This
    function conserve the presence of abscence of velocities.

    :argument integer natoms: the new number of atoms in the frame
    :optional integer status [optional]: The status code

.. f:subroutine:: positions(data, size, [status])

    Get a pointer to the positions array from a frame. The positions are stored as a ``3
    x N`` array, this function set a pointer to point to the first element of this array,
    and give the value of N in the ``size`` argument. If the frame is resized (by writing
    to it, or calling ``chfl_frame%resize``), the pointer is invalidated.

    :argument real data [dimension(\:, \:), pointer]: A pointer to a float array containing the positions
    :argument integer size: After the call, contains the array size (N).
    :optional integer status [optional]: The status code

.. f:subroutine:: velocities(data, size, [status])

    Get a pointer to the velocities array from a frame. The velocities are stored as a ``3
    x N`` array, this function set a pointer to point to the first element of this array,
    and give the value of N in the ``size`` argument. If the frame is resized (by writing
    to it, or calling ``chfl_frame%resize``), the pointer is invalidated.

    :argument real data [dimension(\:, \:), pointer]: A pointer to a float array containing the velocities
    :argument integer size: The array size (N).
    :optional integer status [optional]: The status code

.. f:subroutine:: add_velocities([status])

    Add velocity storage to this frame. The storage is initialized with the result of
    ``chfl_frame%atoms_count`` as number of atoms. If the frame already have velocities,
    this does nothing.

    :optional integer status [optional]: The status code

.. f:subroutine:: has_velocities(has_vel, [status])

    Check if a frame has velocity information.

    :argument logical has_vel: ``.true.`` if the frame has velocities, ``.false.`` otherwise.
    :optional integer status [optional]: The status code

.. f:subroutine:: set_cell(cell, [status])

    Set the UnitCell of a Frame.

    :argument chfl_cell cell: The new unit cell
    :optional integer status [optional]: The status code

.. f:subroutine:: set_topology(topology, [status])

    Set the Topology of a Frame.

    :argument chfl_topology topology: The new topology
    :optional integer status [optional]: The status code

.. f:subroutine:: step(step, [status])

    Get the Frame step, i.e. the frame number in the trajectory

    :argument integer step: This will contains the step number
    :optional integer status [optional]: The status code

.. f:subroutine:: set_step(step, [status])

    Set the Frame step.

    :argument integer step: The new frame step
    :optional integer status [optional]: The status code

.. f:subroutine:: guess_topology(bonds, [status])

    Try to guess the bonds, angles and dihedrals in the system. If ``bonds``
    is ``.true.``, guess everything; else only guess the angles and dihedrals from
    the bond list.

    :argument logical bonds: Should we recompute the bonds from the positions or not ?
    :optional integer status [optional]: The status code


.. f:subroutine:: selection(selection, matched, natoms)

    Select atoms in a frame, from a specific selection string.

    This function select atoms in a frame matching a selection string. For example,
    ``"name H and x > 4"`` will select all the atoms with name ``"H"`` and ``x``
    coordinate less than 4. See the C++ documentation for the full selection language.

    Results of this function are used to fill the ``matched`` pre-allocated array
    containing ``natoms`` logical, where ``natoms`` is the number of atoms in the frame.
    The array will contain ``.true.`` at position ``i`` if the atom at position ``i``
    matches the selection string, and false otherwise.

    :argument integer natoms: The selection string
    :argument logical matched [dimension(\:)]: A pre-allocated array of size ``natoms``
    :argument integer natoms: The size of the ``matched`` array. This must be the same size as ``chfl_frame%atoms_count``
    :optional integer status [optional]: The status code

.. f:subroutine:: free([status])

    Destroy a frame, and free the associated memory

    :optional integer status [optional]: The status code

``chfl_cell`` type
------------------

.. f:currentmodule:: chfl_cell

.. f:type:: chfl_cell

    Wrapping around a C pointer of type ``CHFL_CELL*``. The following
    subroutine are available:

    :field subroutine init:
    :field subroutine from_frame:
    :field subroutine lengths:
    :field subroutine set_lengths:
    :field subroutine angles:
    :field subroutine set_angles:
    :field subroutine matrix:
    :field subroutine type:
    :field subroutine set_type:
    :field subroutine periodicity:
    :field subroutine set_periodicity:
    :field subroutine free:

    The initialization routine are ``init`` and ``from_frame``.


.. f:subroutine:: init(a, b, c, alpha, beta, gamma, [status])

    Create an ``chfl_cell`` from the three lenghts and the three angles.

    :argument real a: the a cell length, in angstroms
    :argument real b: the b cell length, in angstroms
    :argument real c: the c cell length, in angstroms
    :argument real alpha: the alpha angles, in degrees
    :argument real beta: the beta angles, in degrees
    :argument real gamma: the gamma angles, in degrees
    :optional integer status [optional]: The status code

.. f:subroutine:: from_frame_init_(frame, [status])

    Get a copy of the ``chfl_cell`` of a frame.

    :argument chfl_frame frame: the frame
    :optional integer status [optional]: The status code

.. f:subroutine:: lengths(a, b, c, [status])

    Get the cell lenghts.

    :argument real a: the a cell length, in angstroms
    :argument real b: the b cell length, in angstroms
    :argument real c: the c cell length, in angstroms
    :optional integer status [optional]: The status code

.. f:subroutine:: set_lengths(a, b, c, [status])

    Set the unit cell lenghts.

    :argument real a: the new a cell length, in angstroms
    :argument real b: the new b cell length, in angstroms
    :argument real c: the new c cell length, in angstroms
    :optional integer status [optional]: The status code

.. f:subroutine:: angles(alpha, beta, gamma, [status])

    Get the cell angles, in degrees.

    :argument real alpha: the alpha angles, in degrees
    :argument real beta: the beta angles, in degrees
    :argument real gamma: the gamma angles, in degrees
    :optional integer status [optional]: The status code

.. f:subroutine:: set_angles(alpha, beta, gamma, [status])

    Set the cell angles, in degrees

    :argument real alpha: the new alpha angles, in degrees
    :argument real beta: the new beta angles, in degrees
    :argument real gamma: the new gamma angles, in degrees
    :optional integer status [optional]: The status code

.. f:subroutine:: matrix(matrix, [status])

    Get the unit cell matricial representation, i.e. the representation of the three
    base vectors arranged as:

    .. code-block:: sh

        | a_x b_x c_x |
        |  0  b_y c_y |
        |  0   0  c_z |


    :argument real matrix [dimension(3, 3)]: the matrix to fill.
    :optional integer status [optional]: The status code

.. f:subroutine:: type(type, [status])

    Get the cell type

    :argument integer type [kind=CHFL_CELL_TYPES]: the type of the cell
    :optional integer status [optional]: The status code

    The cell types are integers which ``kind`` is the parameter ``CHFL_CELL_TYPES``:

    .. f:variable:: integer(CHFL_CELL_TYPES) :: CHFL_CELL_ORTHOROMBIC

        The three angles are 90°

    .. f:variable:: integer(CHFL_CELL_TYPES) :: CHFL_CELL_TRICLINIC

        The three angles may not be 90°

    .. f:variable:: integer(CHFL_CELL_TYPES) :: CHFL_CELL_INFINITE

        Cell type when there is no periodic boundary conditions

.. f:subroutine:: set_type(type, [status])

    Set the cell type

    :argument integer type [kind=CHFL_CELL_TYPES]: the new type of the cell
    :optional integer status [optional]: The status code

.. f:subroutine:: free([status])

    Destroy an unit cell, and free the associated memory

    :optional integer status [optional]: The status code

``chfl_topology`` type
----------------------

.. f:currentmodule:: chfl_topology

.. f:type:: chfl_topology

    Wrapping around a C pointer of type ``CHFL_TOPOLOGY*``. The following
    subroutine are available:

    :field subroutine init:
    :field subroutine from_frame:
    :field subroutine atoms_count:
    :field subroutine guess:
    :field subroutine append:
    :field subroutine remove:
    :field subroutine isbond:
    :field subroutine isangle:
    :field subroutine isdihedral:
    :field subroutine bonds_count:
    :field subroutine angles_count:
    :field subroutine dihedrals_count:
    :field subroutine bonds:
    :field subroutine angles:
    :field subroutine dihedrals:
    :field subroutine add_bond:
    :field subroutine remove_bond:
    :field subroutine free:

    The initialization routine are ``init`` and ``from_frame``.

.. f:subroutine:: init([status])

    Create a new empty topology

    :optional integer status [optional]: The status code

.. f:subroutine:: from_frame(frame, [status])

    Extract the topology from a frame.

    :argument chfl_frame frame: The frame
    :optional integer status [optional]: The status code

.. f:subroutine:: atoms_count(natoms, [status])

    Get the current number of atoms in the topology.

    :argument integer natoms: Will contain the number of atoms in the frame
    :optional integer status [optional]: The status code

.. f:subroutine:: append(atom, [status])

    Add an atom at the end of a topology.

    :argument chfl_atom atom: The atom to be added
    :optional integer status [optional]: The status code


.. f:subroutine:: remove(i, [status])

    Remove an atom from a topology by index.

    :argument integer i: The atomic index
    :optional integer status [optional]: The status code

.. f:subroutine:: isbond(i, j, result, [status])

    Tell if the atoms ``i`` and ``j`` are bonded together

    :argument integer i: The atomic index of the first atom
    :argument integer j: The atomic index of the second atom
    :argument logical result: ``.true.`` if the atoms are bonded, ``.false.`` otherwise
    :optional integer status [optional]: The status code

.. f:subroutine:: isangle(i, j, k, result, [status])

    Tell if the atoms ``i``, ``j`` and ``k`` constitues an angle

    :argument integer i: The atomic index of the first atom
    :argument integer j: The atomic index of the second atom
    :argument integer k: The atomic index of the third atom
    :argument logical result: ``.true.`` if the atoms constitues an angle, ``.false.`` otherwise
    :optional integer status [optional]: The status code

.. f:subroutine:: isdihedral(i, j, k, m, result, [status])

    Tell if the atoms ``i``, ``j``, ``k`` and ``m`` constitues a dihedral angle

    :argument integer i: The atomic index of the first atom
    :argument integer j: The atomic index of the second atom
    :argument integer k: The atomic index of the third atom
    :argument integer m: The atomic index of the fourth atom
    :argument logical result: ``.true.`` if the atoms constitues a dihedral angle, ``.false.`` otherwise
    :optional integer status [optional]: The status code

.. f:subroutine:: bonds_count(nbonds, [status])

    Get the number of bonds in the system

    :argument integer nbonds: After the call, contains the number of bond
    :optional integer status [optional]: The status code

.. f:subroutine:: angles_count(nangles, [status])

    Get the number of angles in the system

    :argument integer nangles: After the call, contains the number of angles
    :optional integer status [optional]: The status code

.. f:subroutine:: dihedrals_count(ndihedrals, [status])

    Get the number of dihedral angles in the system

    :argument integer ndihedrals: After the call, contains the number of dihedral angles
    :optional integer status [optional]: The status code

.. f:subroutine:: bonds(data, nbonds, [status])

    Get the bonds in the system

    :argument integer data [dimension(2, nbonds)]: A 2x ``nbonds`` array to be
                                            filled with the bonds in the system
    :argument integer nbonds: The size of the array. This should equal the value
                                given by the ``chfl_topology%bonds_count`` function
    :optional integer status [optional]: The status code

.. f:subroutine:: angles(data, nangles, [status])

    Get the angles in the system

    :argument integer data [dimension(3, nangles)]: A 3x ``nangles`` array to be
                                            filled with the angles in the system
    :argument integer nangles: The size of the array. This should equal the
                        value give by the ``chfl_topology%angles_count`` function
    :optional integer status [optional]: The status code

.. f:subroutine:: dihedrals(data, ndihedrals, [status])

    Get the dihedral angles in the system

    :argument integer data [dimension(4, ndihedrals)]: A 4x ``ndihedrals`` array
                            to be filled with the dihedral angles in the system
    :argument integer ndihedrals: The size of the array. This should equal the
                    value give by the ``chfl_topology%dihedrals_count`` function
    :optional integer status [optional]: The status code

.. f:subroutine:: add_bond(i, j, [status])

    Add a bond between the atoms ``i`` and ``j`` in the system

    :argument integer i: The atomic index of the first atom
    :argument integer j: The atomic index of the second atom
    :optional integer status [optional]: The status code

.. f:subroutine:: remove_bond(i, j, [status])

    Remove any existing bond between the atoms ``i`` and ``j`` in the system

    :argument integer i: The atomic index of the first atom
    :argument integer j: The atomic index of the second atom
    :optional integer status [optional]: The status code

.. f:subroutine:: free([status])

    Destroy a topology, and free the associated memory

    :optional integer status [optional]: The status code

``chfl_atom`` type
------------------

.. f:currentmodule:: chfl_atom

.. f:type:: chfl_atom

    Wrapping around a C pointer of type ``CHFL_ATOM*``. The following
    subroutine are available:

    :field subroutine init:
    :field subroutine from_frame:
    :field subroutine from_topology:
    :field subroutine mass:
    :field subroutine set_mass:
    :field subroutine charge:
    :field subroutine set_charge:
    :field subroutine name:
    :field subroutine set_name:
    :field subroutine full_name:
    :field subroutine vdw_radius:
    :field subroutine covalent_radius:
    :field subroutine atomic_number:
    :field subroutine free:

    The initialization routine are ``init``, ``from_frame`` and ``from_topology``.

.. f:subroutine:: init(name, [status])

    Create an atom from an atomic name

    :argument string name: The new atom name
    :optional integer status [optional]: The status code

.. f:subroutine:: from_frame(frame, idx, [status])

    Get a specific atom from a frame

    :argument chfl_frame frame: The frame
    :argument integer idx: The atom index in the frame
    :optional integer status [optional]: The status code

.. f:subroutine:: from_topology(topology, idx, [status])

    Get a specific atom from a topology

    :argument chfl_topology topology: The topology
    :argument integer idx: The atom index in the topology
    :optional integer status [optional]: The status code

.. f:subroutine:: mass(mass, [status])

    Get the mass of an atom, in atomic mass units

    :argument real mass: The atom mass
    :optional integer status [optional]: The status code

.. f:subroutine:: set_mass(mass, [status])

    Set the mass of an atom, in atomic mass units

    :argument real mass: The new atom mass
    :optional integer status [optional]: The status code

.. f:subroutine:: charge(charge, [status])

    Get the charge of an atom, in number of the electron charge e

    :argument real charge: The atom charge
    :optional integer status [optional]: The status code

.. f:subroutine:: set_charge(charge, [status])

    Set the charge of an atom, in number of the electron charge e

    :argument real charge: The new atom charge
    :optional integer status [optional]: The status code

.. f:subroutine:: name(name, buffsize, [status])

    Get the name of an atom

    :argument string name: A string buffer to be filled with the name
    :argument buffsize: The lenght of the string ``name``
    :optional integer status [optional]: The status code

.. f:subroutine:: set_name(name, [status])

    Set the name of an atom

    :argument string name: A string containing the new name
    :optional integer status [optional]: The status code

.. f:subroutine:: full_name(name, buffsize, [status])

    Try to get the full name of an atom from the short name

    :argument string name: A string buffer to be filled with the name
    :argument buffsize: The lenght of the string ``name``
    :optional integer status [optional]: The status code

.. f:subroutine:: vdw_radius(radius, [status])

    Try to get the Van der Waals radius of an atom from the short name

    :argument real radius: The Van der Waals radius of the atom or -1 if no value could be found.
    :optional integer status [optional]: The status code

.. f:subroutine:: covalent_radius(radius, [status])

    Try to get the covalent radius of an atom from the short name

    :argument real radius: The covalent radius of the atom or -1 if no value could be found.
    :optional integer status [optional]: The status code

.. f:subroutine:: atomic_number(number, [status])

    Try to get the atomic number of an atom from the short name

    :argument integer number: The atomic number, or -1 if no value could be found.
    :optional integer status [optional]: The status code

.. f:subroutine:: type(type, [status])

    Get the atom type

    :argument integer type [kind=CHFL_ATOM_TYPES]: the type of the atom
    :optional integer status [optional]: The status code

    The atom types are integers which ``kind`` is the parameter ``CHFL_ATOM_TYPES``:

    .. f:variable:: integer(CHFL_ATOM_TYPES) :: CHFL_ATOM_ELEMENT

        Element from the periodic table of elements.

    .. f:variable:: integer(CHFL_ATOM_TYPES) :: CHFL_ATOM_COARSE_GRAINED

        Coarse-grained atom are composed of more than one element: CH3 groups,
        amino-acids are coarse-grained atoms.

    .. f:variable:: integer(CHFL_ATOM_TYPES) :: CHFL_ATOM_DUMMY

        Dummy site, with no physical reality.

    .. f:variable:: integer(CHFL_ATOM_TYPES) :: CHFL_ATOM_UNDEFINED

        Undefined atom type.

.. f:subroutine:: set_type(type, [status])

    Set the atom type

    :argument integer type [kind=CHFL_ATOM_TYPES]: the new type of the atom
    :optional integer status [optional]: The status code

.. f:subroutine:: free([status])

    Destroy an atom, and free the associated memory

    :optional integer status [optional]: The status code
