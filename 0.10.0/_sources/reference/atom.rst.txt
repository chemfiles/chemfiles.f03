``chfl_atom`` type
------------------

.. f:type:: chfl_atom

    A :f:type:`chfl_atom` is a particle in the current :f:type:`chfl_frame`.

    All atoms have an atomic name; an atomic type; an atomic mass and an atomic
    charge.

    The atomic name is usually an unique identifier (``H1``, ``C_a``) while the
    atomic type will be shared between all particles of the same type: ``H``,
    ``Ow``, ``CH3``.

    :field subroutine init: :f:func:`chfl_atom%init`
    :field subroutine copy: :f:func:`chfl_atom%copy`
    :field function mass: :f:func:`chfl_atom%mass`
    :field subroutine set_mass: :f:func:`chfl_atom%set_mass`
    :field function charge: :f:func:`chfl_atom%charge`
    :field subroutine set_charge: :f:func:`chfl_atom%set_charge`
    :field function type: :f:func:`chfl_atom%type`
    :field subroutine set_type: :f:func:`chfl_atom%set_type`
    :field function name: :f:func:`chfl_atom%name`
    :field subroutine set_name: :f:func:`chfl_atom%set_name`
    :field function full_name: :f:func:`chfl_atom%full_name`
    :field function vdw_radius: :f:func:`chfl_atom%vdw_radius`
    :field function covalent_radius: :f:func:`chfl_atom%covalent_radius`
    :field function atomic_number: :f:func:`chfl_atom%atomic_number`
    :field subroutine set: :f:func:`chfl_atom%set`
    :field subroutine get: :f:func:`chfl_atom%get`
    :field function properties_count: :f:func:`chfl_atom%properties_count`
    :field function list_properties: :f:func:`chfl_atom%list_properties`
    :field subroutine free: :f:func:`chfl_atom%free`

.. f:subroutine:: chfl_atom%init(name, [status])

    Initialize this atom with the given ``name``, and set the atom type to
    ``name``. This subroutine allocate memory which must be released with
    :f:func:`chfl_atom%free`.

    :argument character(len=\*) name: atom name
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_atom%copy(atom, [status])

    Initialize this atom with a copy of ``atom``. This subroutine allocate
    memory which must be released with :f:func:`chfl_atom%free`.

    :argument type(chfl_atom) atom: atom to copy
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_atom%mass([status])

    Get the mass of the atom in atomic mass units.

    :return real:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_atom%set_mass(mass, [status])

    Set the mass of the atom to ``mass``. The value should be in atomic mass
    units.

    :argument real mass: new atom mass
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_atom%charge([status])

    Get the charge of the atom in number of the electron charge *e*.

    :return real:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_atom%set_charge(charge, [status])

    Set the charge of the atom to ``charge``. The charge should be in number of
    the electron charge *e*.

    :argument real charge: new atom charge
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_atom%name([status])

    Get the name of an atom. If the name is longer than
    :f:var:`CHFL_STRING_LENGTH`, it will be truncated.

    :return character(len=CHFL_STRING_LENGTH):
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_atom%set_name(name, [status])

    Set the name of an atom to ``name``.

    :argument character(len=\*) name: new atom name
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_atom%full_name([status])

    Get the full name of an ``atom`` from its type. If the name is longer than
    :f:var:`CHFL_STRING_LENGTH`, it will be truncated.

    :return character(len=CHFL_STRING_LENGTH):
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_atom%type([status])

    Get the type of an atom. If the name is longer than
    :f:var:`CHFL_STRING_LENGTH`, it will be truncated.

    :return character(len=CHFL_STRING_LENGTH):
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_atom%set_type(type, [status])

    Set the type of an atom to ``type``.

    :argument character(len=\*) name: new atom type
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_atom%vdw_radius([status])

    Get the Van der Waals radius of an atom from the atom type. If the radius in
    unknown, this function returns 0.

    :return real:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_atom%covalent_radius([status])

    Get the covalent radius of an atom from the atom type. If the radius in
    unknown, this function set ``radius`` to 0.

    :return real:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_atom%atomic_number([status])

    Get the atomic number of an atom from the atom type. If the atomic number in
    unknown, this function set ``number`` to 0.

    :argument integer:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_atom%set(name, property, [status])

    Add a new ``property`` with the given ``name`` to this atom.

    If a property with the same name already exists, this function override the
    existing property with the new one.

    ``property`` can either be a :f:type:`chfl_property`, or any value that can
    be stored in a :f:type:`chfl_property`: logical, real, string, or vector3d.

    :argument character(len=\*) name: property name
    :argument type(chfl_property) property: the new property
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_atom%get(name, [status])

    Get a copy of the property with the given ``name`` in this atom. If no
    property exist with this name, ``status`` will be set to
    :f:var:`CHFL_PROPERTY_ERROR`.

    The associated memory must be released by calling
    :f:func:`chfl_property%free`.

    :return type(chfl_property):
    :argument character(len=\*) name: property name
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_atom%properties_count([status])

    Get the number of properties in this atom.

    :return integer:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_atom%list_properties(names, [status])

    Fill the pre-allocated array ``names`` with the names of the properties in
    this atom. The array must have room for :f:func:`chfl_atom%properties_count`
    values of type ``character(len=CHFL_STRING_LENGTH)``.

    :return integer:
    :argument character(len=CHFL_STRING_LENGTH) names(\:): list of properties names
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_atom%free()

    Destroy an atom, and free the associated memory
