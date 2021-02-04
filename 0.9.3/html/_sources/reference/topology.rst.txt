``chfl_topology`` type
----------------------

.. f:type:: chfl_topology

    A :f:type:`chfl_topology` contains the definition of all the atoms in the
    system, and the liaisons between the atoms (bonds, angles, dihedrals,
    ...). It will also contain all the residues information if it is available.

    :field subroutine init: :f:func:`chfl_topology%init`
    :field subroutine copy: :f:func:`chfl_topology%copy`
    :field function atoms_count: :f:func:`chfl_topology%atoms_count`
    :field subroutine add_atom: :f:func:`chfl_topology%add_atom`
    :field function atom: :f:func:`chfl_topology%atom`
    :field subroutine resize: :f:func:`chfl_topology%resize`
    :field subroutine remove: :f:func:`chfl_topology%remove`
    :field subroutine add_bond: :f:func:`chfl_topology%add_bond`
    :field subroutine remove_bond: :f:func:`chfl_topology%remove_bond`
    :field function bond_order: :f:func:`chfl_topology%bond_order`
    :field subroutine bond_orders: :f:func:`chfl_topology%bond_orders`
    :field function bonds_count: :f:func:`chfl_topology%bonds_count`
    :field function angles_count: :f:func:`chfl_topology%angles_count`
    :field function dihedrals_count: :f:func:`chfl_topology%dihedrals_count`
    :field function impropers_count: :f:func:`chfl_topology%impropers_count`
    :field subroutine bonds: :f:func:`chfl_topology%bonds`
    :field subroutine angles: :f:func:`chfl_topology%angles`
    :field subroutine dihedrals: :f:func:`chfl_topology%dihedrals`
    :field subroutine impropers: :f:func:`chfl_topology%impropers`
    :field function residues_count: :f:func:`chfl_topology%residues_count`
    :field subroutine add_residue: :f:func:`chfl_topology%add_residue`
    :field function residue: :f:func:`chfl_topology%residue`
    :field function residue_for_atom: :f:func:`chfl_topology%residue_for_atom`
    :field function residues_linked: :f:func:`chfl_topology%residues_linked`
    :field subroutine free: :f:func:`chfl_topology%free`

.. f:subroutine:: chfl_topology%init([status])

    Initialize this topology with a new empty topology. This subroutine allocate
    memory which must be released with :f:func:`chfl_topology%free`.

    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_topology%copy(topology, [status])

    Initialize this topology with a copy of ``topology``. This subroutine
    allocate memory which must be released with :f:func:`chfl_topology%free`.

    :argument type(chfl_topology) topology: topology to copy
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_topology%atoms_count([status])

    Get the number of atoms in the topology.

    :return integer:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_topology%resize(count, [status])

    Resize the topology to hold ``count`` atoms. If the new number of atoms is
    bigger than the current number, new atoms will be created with an empty name
    and type. If it is lower than the current number of atoms, the last atoms
    will be removed, together with the associated bonds, angles and dihedrals.

    :argument integer count: new size of the topology
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_topology%add_atom(atom, [status])

    Add a copy of ``atom`` at the end of the topology.

    :argument type(chfl_atom) atom: atom to be added
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_topology%atom(index, [status])

    Get access to the atom at the given ``index`` from this topology

    Any modification to the atom will be reflected in the topology. The topology
    will be kept alive, even if :f:func:`chfl_topology%free` is called, until
    :f:func:`chfl_atom%free` is also called on the atom returned by this
    function.

    The atom returned by this function is a pointer that points directly inside
    the topology, and will be invalidated if any of the following function is
    called on the topology:

    - :f:func:`chfl_topology%resize`
    - :f:func:`chfl_topology%add_atom`
    - :f:func:`chfl_topology%remove`

    Calling any function on an invalidated pointer is undefined behavior. Even
    if the pointer if invalidated, it stills needs to be released with
    `chfl_atom%free`.

    :return type(chfl_atom):
    :argument integer index: index of the atom to retrieve
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_topology%remove(index, [status])

    Remove the atom at the given ``index`` from the topology.

    This shifts all the atoms indexes after ``index`` by 1 (n becomes n-1).

    :argument integer index: index of the atom to remove
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_topology%bonds_count([status])

    Get the number of bonds in the topology.

    :return integer:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_topology%angles_count([status])

    Get the number of angles in the topology.

    :return integer:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_topology%dihedrals_count([status])

    Get the number of dihedral angles in the topology.

    :return integer:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_topology%impropers_count([status])

    Get the number of improper dihedral angles in the topology.

    :return integer:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_topology%bonds(data, [status])

    Get the list of bonds in the topology in the pre-allocated array ``data``
    of shape ``[2, chfl_topology%bonds_count()]``.

    :argument integer data(2, \:): array to be filled with the bonds in the system
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_topology%angles(data, [status])

    Get the list of angles in the ``topology`` in the pre-allocated array
    ``data`` of shape ``[3, chfl_topology%angles_count()]``.

    :argument integer data(3, \:): array to be filled with the angles in the system
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_topology%dihedrals(data, [status])

    Get the list of dihedral angles in the topology in the pre-allocated array
    ``data`` of shape ``[4, chfl_topology%dihedrals_count()]``.

    :argument integer data(4, \:): array to be filled with the dihedral angles
        in the system
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_topology%impropers(data, [status])

    Get the list of improper dihedral angles in the topology in the
    pre-allocated array ``data`` of shape ``[4, chfl_topology%impropers_count()]``.

    :argument integer data(4, \:): array to be filled with the dihedral angles
        in the system
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_topology%add_bond(i, j, [bond_order, status])

    Add a bond between the atoms at indexes ``i`` and ``j`` in the topology, and
    optionaly set the ``bond_order``. By default, a bond order of
    :f:var:`CHFL_BOND_UNKNOWN` is used.

    Possible bond orders are represented by an integer of kind :f:var:`chfl_bond_order`.

    :argument integer i: atomic index of the first atom of the bond
    :argument integer j: atomic index of the second atom of the bond
    :optional integer(chfl_bond_order) bond_order: order of the bond
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:variable:: chfl_bond_order
    :type: integer

    Kind parameter for integers representing bond orders.

.. f:variable:: CHFL_BOND_UNKNOWN
    :type: integer(chfl_bond_order)

    This bond order is used when the actual bond order is unkown or unspecified
    in the input file format.

.. f:variable:: CHFL_BOND_SINGLE
    :type: integer(chfl_bond_order)

    Bond order for single bonds

.. f:variable:: CHFL_BOND_DOUBLE
    :type: integer(chfl_bond_order)

    Bond order for double bonds

.. f:variable:: CHFL_BOND_TRIPLE
    :type: integer(chfl_bond_order)

    Bond order for triple bonds

.. f:variable:: CHFL_BOND_QUADRUPLE
    :type: integer(chfl_bond_order)

    Bond order for quadruple bond (present in some metals)

.. f:variable:: CHFL_BOND_QINTUPLET
    :type: integer(chfl_bond_order)

    Bond order for quintuplet bond (present in some metals)

.. f:variable:: CHFL_BOND_AMIDE
    :type: integer(chfl_bond_order)

    Bond order for amide bonds

.. f:variable:: CHFL_BOND_AROMATIC
    :type: integer(chfl_bond_order)

    Bond order for aromatic bonds

.. f:subroutine:: chfl_topology%remove_bond(i, j, [status])

    Remove any existing bond between the atoms at indexes ``i`` and ``j`` in the
    topology.

    This function does nothing if there is no bond between ``i`` and ``j``.

    :argument integer i: The atomic index of the first atom
    :argument integer j: The atomic index of the second atom
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_topology%add_bond(i, j, [bond_order, status])

    Add a bond between the atoms at indexes ``i`` and ``j`` in the topology, and
    optionaly set the ``bond_order``. By default, a bond order of
    :f:var:`CHFL_BOND_UNKOWN` is used.

    Possible bond orders are represented by an integer of kind :f:var:`chfl_bond_order`.

    :argument integer i: atomic index of the first atom of the bond
    :argument integer j: atomic index of the second atom of the bond
    :optional integer(chfl_bond_order) bond_order: order of the bond
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_topology%bond_order(i, j, [status])

    Get the bond order for the bond between atoms at indexes ``i`` and ``j``.

    :return integer(chfl_bond_order):
    :argument integer i: atomic index of the first atom of the bond
    :argument integer j: atomic index of the second atom of the bond
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_topology%bond_orders(data, [status])

    Get the bond order for all bonds in the system in the pre-allocated array
    ``data``. This array must have room for :f:func:`chfl_topology%bonds_count`
    elements.

    :argument integer(chfl_bond_order) data(\:): Array to be filled with the
        bond order of all bonds in the system.
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_topology%residues_count(natoms, [status])

    Get the number of residues in the topology.

    :return integer:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_topology%add_residue(residue, [status])

    Add a copy of ``residue`` to this topology.

    The residue id must not already be in the topology, and the residue must
    contain only atoms that are not already in another residue.

    :argument type(chfl_residue) residue: residue to add in the topology
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_topology%residue(index, [status])

    Get read-only access to the residue at the given ``index`` in this topology.
    Trying to write to the residue will give an error.

    If the ``index`` is bigger than the result of
    :f:func:`chfl_topology%residues_count`, this function will return an invalid
    :f:type:`chfl_residue` and set ``status`` to ``CHFL_MEMORY_ERROR``.

    The residue index in the topology is not always the same as the residue
    id.

    The topology will be kept alive, even if :f:func:`chfl_topology%free` is
    called, until :f:func:`chfl_residue%free` is also called on the residue
    returned by this function, unless the this function returns an invalid
    residue.

    The residue returned by this function is a pointer that points directly
    inside the topology, and will be invalidated if
    :f:func:`chfl_topology%add_residue` is called. Calling any function on an
    invalidated pointer is undefined behavior. Even if the pointer if
    invalidated, it stills needs to be released with
    :f:func:`chfl_residue%free`.

    :return type(chfl_residue):
    :argument integer index: Index of the residue in the topology. This is NOT
        the same thing as the residue id.
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_topology%residue_for_atom(index, [status])

    Get read-only access to the residue containing the atom at the given
    ``index`` in this topology. Trying to write to the residue will give an
    error.

    This function will return an invalid invalid :f:type:`chfl_residue` and set
    ``status`` to ``CHFL_MEMORY_ERROR`` if the atom is not in a residue, or if
    the ``index`` is bigger than :f:func:`chfl_topology%atoms_count`.

    The topology will be kept alive, even if :f:func:`chfl_topology%free` is
    called, until :f:func:`chfl_residue%free` is also called on the residue
    returned by this function.

    The residue returned by this function is a pointer that points directly
    inside the topology, and will be invalidated if
    :f:func:`chfl_topology%add_residue` is called. Calling any function on an
    invalidated pointer is undefined behavior. Even if the pointer if
    invalidated, it stills needs to be released with
    :f:func:`chfl_residue%free`.

    :return type(chfl_residue):
    :argument integer index: index of the atom in the topology.
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_topology%residues_linked(first, second, [status])

    Check if the two residues ``first`` and ``second`` from the topology are
    linked together, *i.e.* if there is a bond between one atom in the first
    residue and one atom in the second one.

    :return logical:
    :argument type(chfl_residue) first: first residue
    :argument type(chfl_residue) second: second residue
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_topology%free()

    Destroy a topology, and free the associated memory
