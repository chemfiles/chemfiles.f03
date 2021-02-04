``chfl_residue`` type
----------------------

.. f:type:: chfl_residue

    A :f:type:`chfl_residue` is a group of atoms belonging to the same logical
    unit. They can be small molecules, amino-acids in a protein, monomers in
    polymers, *etc.*

    :field subroutine init: :f:func:`chfl_residue%init`
    :field subroutine copy: :f:func:`chfl_residue%copy`
    :field function name: :f:func:`chfl_residue%name`
    :field function id: :f:func:`chfl_residue%id`
    :field function atoms_count: :f:func:`chfl_residue%atoms_count`
    :field subroutine add_atom: :f:func:`chfl_residue%add_atom`
    :field function contains: :f:func:`chfl_residue%contains`
    :field subroutine atoms: :f:func:`chfl_residue%atoms`
    :field subroutine set: :f:func:`chfl_residue%set`
    :field function get: :f:func:`chfl_residue%get`
    :field function properties_count: :f:func:`chfl_residue%properties_count`
    :field subroutine list_properties: :f:func:`chfl_residue%list_properties`
    :field subroutine free: :f:func:`chfl_residue%free`

.. f:subroutine:: chfl_residue%init(name, [id, status])

    Initialize the residue with a new residue with the given ``name`` and
    optional residue ``id``. This subroutine allocate memory which must be
    released with :f:func:`chfl_residue%free`.

    :argument character(len=\*) name: residue name
    :optional integer id: residue id
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_residue%copy(residue, [status])

    Initialize this residue with a copy of ``residue``. This subroutine allocate
    memory which must be released with :f:func:`chfl_residue%free`.

    :argument type(chfl_residue) residue: residue to copy
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_residue%name([status])

    Get the name of the residue. If the name is longer than
    :f:var:`CHFL_STRING_LENGTH`, it will be truncated.

    :return character(len=CHFL_STRING_LENGTH):
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_residue%id([status])

    Get the residue id. If the residue do not have an id, ``status`` is set to
    :f:var:`CHFL_GENERIC_ERROR`, and no value is returned.

    :return integer:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_residue%atoms_count([status])

    Get the number of atoms in the residue.

    :return integer:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_residue%add_atom(index, [status])

    Add the atom at the given ``index`` in the residue.

    :argument integer index: index of the atom to add
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_residue%contains(index, [status])

    Check if the atom at the given ``index`` is in the residue.

    :return logical:
    :argument integer index: index of the atom
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_residue%atoms(data, [status])

    Get the list of atoms in the topology in the pre-allocated array ``data``
    of size :f:func:`chfl_residue%atoms_count`.

    :argument integer data [dimension(\:)]: array to be filled with
        the indexes of atoms in the residue
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_residue%set(name, property, [status])

    Add a new ``property`` with the given ``name`` to this residue.

    If a property with the same name already exists, this function override the
    existing property with the new one.

    ``property`` can either be a :f:type:`chfl_property`, or any value that can
    be stored in a :f:type:`chfl_property`: logical, real, string, or vector3d.

    :argument character(len=\*) name: property name
    :argument type(chfl_property) property: the new property
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_residue%get(name, [status])

    Get a copy of the property with the given ``name`` in this residue. If no
    property exist with this name, ``status`` will be set to
    :f:var:`CHFL_PROPERTY_ERROR`.

    The associated memory must be released by calling
    :f:func:`chfl_property%free`.

    :return type(chfl_property):
    :argument character(len=\*) name: property name
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_residue%properties_count([status])

    Get the number of properties in this residue.

    :return integer:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_residue%list_properties(names, [status])

    Fill the pre-allocated array ``names`` with the names of the properties in
    this residue. The array must have room for
    :f:func:`chfl_residue%properties_count` values of type
    ``character(len=CHFL_STRING_LENGTH)``.

    :return integer:
    :argument character(len=CHFL_STRING_LENGTH) names(\:): list of properties names
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_residue%free()

    Destroy a residue, and free the associated memory
