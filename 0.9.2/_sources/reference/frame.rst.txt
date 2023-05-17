
``chfl_frame`` type
-------------------

.. f:type:: chfl_frame

    A :f:type:`chfl_frame` contains data from one simulation step: the current unit
    cell, the topology, the positions, and the velocities of the particles in
    the system. If some information is missing (topology or velocity or unit
    cell), the corresponding data is filled with a default value.

    :field subroutine init: :f:func:`chfl_frame%init`
    :field subroutine copy: :f:func:`chfl_frame%copy`
    :field function atoms_count: :f:func:`chfl_frame%atoms_count`
    :field subroutine add_atom: :f:func:`chfl_frame%add_atom`
    :field function atom: :f:func:`chfl_frame%atom`
    :field subroutine remove: :f:func:`chfl_frame%remove`
    :field subroutine resize: :f:func:`chfl_frame%resize`
    :field subroutine add_bond: :f:func:`chfl_frame%add_bond`
    :field subroutine remove_bond: :f:func:`chfl_frame%remove_bond`
    :field subroutine add_residue: :f:func:`chfl_frame%add_residue`
    :field function distance: :f:func:`chfl_frame%distance`
    :field function angle: :f:func:`chfl_frame%angle`
    :field function dihedral: :f:func:`chfl_frame%dihedral`
    :field function out_of_plane: :f:func:`chfl_frame%out_of_plane`
    :field function positions: :f:func:`chfl_frame%positions`
    :field function velocities: :f:func:`chfl_frame%velocities`
    :field subroutine add_velocities: :f:func:`chfl_frame%add_velocities`
    :field subroutine has_velocities: :f:func:`chfl_frame%has_velocities`
    :field subroutine cell: :f:func:`chfl_frame%cell`
    :field subroutine set_cell: :f:func:`chfl_frame%set_cell`
    :field function topology: :f:func:`chfl_frame%topology`
    :field subroutine set_topology: :f:func:`chfl_frame%set_topology`
    :field subroutine guess_bonds: :f:func:`chfl_frame%guess_bonds`
    :field function step: :f:func:`chfl_frame%step`
    :field subroutine set_step: :f:func:`chfl_frame%set_step`
    :field subroutine set: :f:func:`chfl_frame%set`
    :field function get: :f:func:`chfl_frame%get`
    :field function properties_count: :f:func:`chfl_frame%properties_count`
    :field subroutine list_properties: :f:func:`chfl_frame%list_properties`
    :field subroutine free: :f:func:`chfl_frame%free`

.. f:subroutine:: chfl_frame%init([status])

    Initialize this frame as an empty frame. This subroutine allocate memory
    which must be released with :f:func:`chfl_frame%free`.

    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_frame%copy(frame, [status])

    Initialize this frame with a copy of ``frame``. This subroutine allocate
    memory which must be released with :f:func:`chfl_frame%free`.

    :argument type(chfl_frame) frame: frame to copy
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_frame%atoms_count([status])

    Get the current number of atoms in the frame in ``natoms``.

    :argument integer natoms: number of atoms in the frame
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_frame%add_atom(atom, position, [velocity, status])

    Add a :f:type:`chfl_atom` and the corresponding ``position`` and
    ``velocity`` data to this frame. ``velocity`` can be absent if no velocity
    is associated with this frame.

    :argument type(chfl_atom) atom: atom to add to the frame
    :argument real position(3): atom position
    :optional real velocity(3) [optional]: atom velocity
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_frame%atom(index, [status])

    Get access to the atom at the given ``index`` in this frame.

    Any modification to the atom will be reflected in the frame. The frame will
    be kept alive, even if :f:func:`chfl_frame%free` is called, until
    :f:func:`chfl_atom%free` is also called on the atom returned by this
    function.

    The atom returned by this function is a pointer that points directly inside
    the frame, and will be invalidated if any of the following function is
    called on the frame:

    - :f:func:`chfl_frame%resize`
    - :f:func:`chfl_frame%add_atom`
    - :f:func:`chfl_frame%remove`
    - :f:func:`chfl_frame%set_topology`

    Calling any function on an invalidated pointer is undefined behavior. Even
    if the pointer if invalidated, it stills needs to be released with
    :f:func:`chfl_atom%free`.

    :return type(chfl_atom):
    :argument integer index: Index of the atom in the frame
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_frame%remove(index, [status])

    Remove the atom at the given ``index`` in the frame.

    This modify all the atoms indexes after ``index``, and invalidate any
    pointer obtained using :f:func:`chfl_frame%positions` or
    :f:func:`chfl_frame%velocities`.

    :argument integer index: index of the atom to remove
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_frame%resize(natoms, [status])

    Resize the positions, velocities and topology in the frame, to have space
    for ``natoms`` atoms.

    This function may invalidate any pointer to the positions or the velocities
    if the new size is bigger than the old one. In all the cases, previous data
    is conserved. This function conserve the presence or absence of velocities.

    :argument integer natoms: the new number of atoms in the frame
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_frame%add_bond(i, j, [bond_order, status])

    Add a bond between the atoms at indexes ``i`` and ``j`` in the frame's
    topology, and optionaly set the ``bond_order``. By default, a bond order of
    :f:var:`CHFL_BOND_UNKNOWN` is used.

    Possible bond orders are represented by an integer of kind
    :f:var:`chfl_bond_order`.

    :argument integer i: atomic index of the first atom of the bond
    :argument integer j: atomic index of the second atom of the bond
    :optional integer(chfl_bond_order) bond_order: order of the bond
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_frame%remove_bond(i, j, [status])

    Remove any existing bond between the atoms at indexes ``i`` and ``j`` in the
    frame's topology.

    This function does nothing if there is no bond between ``i`` and ``j``.

    :argument integer i: atomic index of the first atom
    :argument integer j: atomic index of the second atom
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_frame%add_residue(residue, [status])

    Add a copy of ``residue`` to this frame's topology.

    The residue id must not already be in the topology, and the residue must
    contain only atoms that are not already in another residue.

    :argument type(chfl_residue) residue: residue to add in the topology
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_frame%distance(i, j, [status])

    Get the distance between the atoms at indexes ``i`` and ``j`` in this frame,
    accounting for periodic boundary conditions and expressed in angstroms.

    :return real:
    :argument integer i: atomic index of the first atom of the pair
    :argument integer j: atomic index of the second atom of the pair
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.


.. f:function:: chfl_frame%angle(i, j, k, [status])

    Get the angle formed by the atoms at indexes ``i``,  ``j`` and ``k`` in this
    frame, accounting for periodic boundary conditions, and expressed in
    radians.

    :return real:
    :argument integer i: atomic index of the first atom of the angle
    :argument integer j: atomic index of the second atom of the angle
    :argument integer k: atomic index of the third atom of the angle
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_frame%dihedral(i, j, k, m, [status])

    Get the dihedral angle formed by the atoms at indexes ``i``,  ``j``,  ``k``
    and ``m`` in this frame, accounting for periodic boundary conditions, and
    expressed in radians.

    :return real:
    :argument integer i: atomic index of the first atom of the angle
    :argument integer j: atomic index of the second atom of the angle
    :argument integer k: atomic index of the third atom of the angle
    :argument integer m: atomic index of the fourth atom of the angle
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_frame%out_of_plane(i, j, k, m, [status])

    Get the out of plane distance formed by the atoms at indexes ``i``, ``j``,
    ``k`` and ``m`` in this frame, accounting for periodic boundary conditions.
    The result is expressed in angstroms.

    This is the distance between the atom *j* and the *ikm* plane. The atom *j*
    is the center of the improper dihedral angle formed by *i*, *j*, *k* and
    *m*.

    :return real:
    :argument integer i: atomic index of the first atom of the angle
    :argument integer j: atomic index of the second atom of the angle
    :argument integer k: atomic index of the third atom of the angle
    :argument integer m: atomic index of the fourth atom of the angle
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_frame%positions(data, size, [status])

    Get a pointer to the positions array from the frame.

    This function gives access to chemfiles internal data structure, and do not
    perform any copy, both when reading and writing the positions.

    The pointer is invalidated if any of the following function is called:
        - :f:func:`chfl_frame%resize`,
        - :f:func:`chfl_frame%remove`,
        - :f:func:`chfl_frame%add_atom`

    When the memory of the frame is released (by calling
    :f:func:`chfl_frame%free`), the pointer is released too.

    This function returns a Fortran pointer, which must be used in a pointer
    assignement context:

    .. code-block :: fortran

        program example
            use iso_fortran_env, only: real64
            use chemfiles
            implicit none
            type(chfl_frame)      :: frame
            real(real64), pointer :: positions(:, :)

            call frame%init()
            call frame%resize(122)

            positions => frame%positions()

            call frame%free()
        end program

    :return real(3, \:) [pointer]:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_frame%velocities([status])

    Get a pointer to the velocities array from the frame.

    This function gives access to chemfiles internal data structure, and do not
    perform any copy, both when reading and writing the velocities.

    The pointer is invalidated if any of the following function is called:
        - :f:func:`chfl_frame%resize`,
        - :f:func:`chfl_frame%remove`,
        - :f:func:`chfl_frame%add_atom`

    When the memory of the frame is released (by calling
    :f:func:`chfl_frame%free`), the pointer is released too.

    This function returns a Fortran pointer, which must be used in a pointer
    assignement context:

    .. code-block :: fortran

        program example
            use iso_fortran_env, only: real64
            use chemfiles
            implicit none
            type(chfl_frame)      :: frame
            real(real64), pointer :: velocities(:, :)

            call frame%init()
            call frame%resize(122)
            call frame%add_velocities()

            velocities => frame%velocities()

            call frame%free()
        end program

    :return real(3, \:) [pointer]:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_frame%add_velocities([status])

    Add velocity data to this frame.

    The velocities ar initialized to zero. If the frame already has velocities,
    this does nothing.

    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_frame%has_velocities([status])

    Check if this frame contains velocity data.

    :return logical:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_frame%cell([status])

    Get access to the cell of this frame

    Any modification to the cell will be reflected in the frame. The frame will
    be kept alive, even if :f:func:`chfl_frame%free` is called, until
    :f:func:`chfl_cell%free` is also called on the cell returned by this
    function.

    If :f:func:`chfl_frame%set_cell` is called, the cell returned by this
    function will point to the new cell.

    :return type(chfl_cell):
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_frame%set_cell(cell, [status])

    Set the :f:type:`chfl_cell` of this frame to ``cell``.

    :argument type(chfl_cell) cell: new unit cell of the frame
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_frame%topology([status])

    Get read-only access to the topology of this frame. Trying to write to the
    topology will give an error.

    The frame will be kept alive, even if :f:func:`chfl_frame%free` is called,
    until :f:func:`chfl_topology%free` is also called on the topology returned by
    this function.

    If :f:func:`chfl_frame%set_topology` is called, the topology returned by
    this function will point to the new topology.

    :return type(chfl_topology):
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_frame%set_topology(topology, [status])

    Set the :f:type:`chfl_topology` of this frame to ``topology``.

    Calling this function with a topology that does not contain the right number
    of atom will return an error.

    :argument type(chfl_topology) topology: new topology of the frame
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_frame%step([status])

    Get the frame step, *i.e.* the frame number in the trajectory.

    :return integer:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_frame%set_step(step, [status])

    Set the frame step, *i.e.* the frame number in the trajectory to ``step``.

    :argument integer step: The new frame step
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_frame%guess_bonds([status])

    Guess the bonds, angles and dihedrals in the frame.

    The bonds are guessed using a distance-based algorithm, and then angles and
    dihedrals are guessed from the bonds.

    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_frame%set(name, property, [status])

    Add a new ``property`` with the given ``name`` to this frame.

    If a property with the same name already exists, this function override the
    existing property with the new one.

    ``property`` can either be a :f:type:`chfl_property`, or any value that can
    be stored in a :f:type:`chfl_property`: logical, real, string, or vector3d.

    :argument character(len=\*) name: property name
    :argument type(chfl_property) property: the new property
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_frame%get(name, [status])

    Get a copy of the property with the given ``name`` in this frame. If no
    property exist with this name, ``status`` will be set to
    :f:var:`CHFL_PROPERTY_ERROR`.

    The associated memory must be released by calling
    :f:func:`chfl_property%free`.

    :return type(chfl_property):
    :argument character(len=\*) name: property name
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_frame%properties_count([status])

    Get the number of properties in this frame.

    :return integer:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_frame%list_properties(names, [status])

    Fill the pre-allocated array ``names`` with the names of the properties in
    this frame. The array must have room for :f:func:`chfl_frame%properties_count`
    values of type ``character(len=CHFL_STRING_LENGTH)``.

    :return integer:
    :argument character(len=CHFL_STRING_LENGTH) names(\:): list of properties names
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_frame%free()

    Destroy a frame, and free the associated memory
