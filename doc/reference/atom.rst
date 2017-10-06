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
