``chfl_property`` type
----------------------

.. f:type:: chfl_property

    A :f:type:`chfl_property` holds data used in properties in
    :f:type:`chfl_frame` and :f:type:`chfl_atom`. A property can contain data of
    various types: bool, real, string or 3 dimensional array.

    The initialization routines for :f:type:`chfl_property` are:

    - :f:func:`chfl_property%bool`;
    - :f:func:`chfl_property%double`;
    - :f:func:`chfl_property%string`;
    - :f:func:`chfl_property%vector3d`;
    - :f:func:`chfl_property%from_atom`;
    - :f:func:`chfl_property%from_frame`;

    :field subroutine from_frame: :f:func:`chfl_property%from_frame`
    :field subroutine from_atom: :f:func:`chfl_property%from_atom`
    :field subroutine bool: :f:func:`chfl_property%bool`
    :field subroutine double: :f:func:`chfl_property%double`
    :field subroutine string: :f:func:`chfl_property%string`
    :field subroutine vector3d: :f:func:`chfl_property%vector3d`
    :field subroutine get_kind: :f:func:`chfl_property%get_kind`
    :field subroutine get_bool: :f:func:`chfl_property%get_bool`
    :field subroutine get_double: :f:func:`chfl_property%get_double`
    :field subroutine get_string: :f:func:`chfl_property%get_string`
    :field subroutine get_vector3d: :f:func:`chfl_property%get_vector3d`
    :field subroutine free: :f:func:`chfl_property%free`


.. f:subroutine:: chfl_property%from_frame(frame, name, [status])

    Try to get a copy of property with the given ``name`` in the given
    ``frame``.

    If no property with this name is found, this property is left uninitialized,
    and ``status`` contains ``CHFL_PROPERTY_ERROR``.

    :argument type(chfl_frame) frame: the frame to look into
    :argument character(len=*) name: the property name
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_property%from_atom(atom, name, [status])

    Try to get a copy of property with the given ``name`` in the given
    ``atom``.

    If no property with this name is found, this property is left uninitialized,
    and ``status`` contains ``CHFL_PROPERTY_ERROR``.

    :argument type(chfl_atom) atom: the frame to look into
    :argument character(len=*) name: the property name
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.


.. f:subroutine:: chfl_property%bool(value, [status])

    Initialize this property with a boolean (logical) `value`.

    :argument logical value [kind=1]: the value of the property
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_property%double(value, [status])

    Initialize this property with a real `value`.

    :argument real value [kind=real64]: the value of the property
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_property%string(value, [status])

    Initialize this property with a string `value`.

    :argument character(len=*) value: the value of the property
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_property%vector3d(value, [status])

    Initialize this property with a 3 dimensional array `value`.

    :argument real value(3) [kind(real64)]: the value of the property
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_property%get_kind(kind, [status])

    Get the property kind in ``kind``. This kind indicate which type of value
    this property holds.

    :argument integer kind [kind=chfl_property_kind]: the kind of property
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

    The property kinds are integers which ``kind`` is the ``chfl_property_kind``
    parameter:

    .. f:variable:: integer(chfl_property_kind) :: CHFL_PROPERTY_BOOL

        This property contain a boolean (logical) value

    .. f:variable:: integer(chfl_property_kind) :: CHFL_PROPERTY_DOUBLE

        This property contain a double (real) value

    .. f:variable:: integer(chfl_property_kind) :: CHFL_PROPERTY_STRING

        This property contain a string value

    .. f:variable:: integer(chfl_property_kind) :: CHFL_PROPERTY_VECTOR3D

        This property contain a 3 dimensional array of real value

.. f:subroutine:: chfl_property%get_bool(value, [status])

    Get the boolean value holded by this property in ``value``

    :argument logical value [kind(1)]: the value of the property
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_property%get_double(value, [status])

    Get the real value holded by this property in `value`.

    :argument real value [kind=real64]: the value of the property
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_property%get_string(value, buffsize, [status])

    Get the string value of this property in the string buffer ``value``.

    The buffer size must be passed in ``buffsize``. This function will truncate
    the name to fit in the buffer.

    :argument character(len=buffsize) name: string buffer to be filled with
        the property value
    :argument integer buffsize: length of the string buffer
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_property%get_vector3d(value, [status])

    Get the real array value holded by this property in `value`.

    :argument real value(3) [kind(real64)]: the value of the property
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_property%free([status])

    Destroy a property, and free the associated memory

    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.
