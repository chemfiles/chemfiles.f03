``chfl_property`` type
----------------------

.. f:type:: chfl_property

    A :f:type:`chfl_property` holds data used in properties in
    :f:type:`chfl_frame`, :f:type:`chfl_atom` and :f:type:`chfl_residue`.

    A property is associated with a name and can contain data of various types:
    logical, real, string or 3 dimensional array (``vector3d``). The property
    kind is used to know which kind of data is stored in the property.

    :field subroutine init: :f:func:`chfl_property%init`
    :field function kind: :f:func:`chfl_property%kind`
    :field function bool: :f:func:`chfl_property%bool`
    :field function double: :f:func:`chfl_property%double`
    :field function string: :f:func:`chfl_property%string`
    :field function vector3d: :f:func:`chfl_property%vector3d`
    :field subroutine free: :f:func:`chfl_property%free`

.. f:subroutine:: chfl_property%init(value, [status])

    This is a generic subroutine for initialization of a property. It allocate
    memory which must be released with :f:func:`chfl_property%free`.

    ``value`` can be a real number, a logical value, a string
    (``string``) or an 3-dimensional array of real numbers.

    :optional integer(chfl_status) status: status code of the operation. If it is not
        :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn more
        about the error.

.. f:function:: chfl_property%kind([status])

    Get the property kind, a number indicating which type of value this property
    holds. The possible kinds are represented by integer of the
    :f:var:`chfl_property_kind` kind.

    .. f:variable:: chfl_property_kind
        :type: integer

        Kind parameter for the integer values describing :f:type:`chfl_property`
        kinds.

    .. f:variable:: CHFL_PROPERTY_BOOL
        :type: integer(chfl_property_kind)

        Kind used for ``logical`` (``bool`` in C++) values.

    .. f:variable:: CHFL_PROPERTY_DOUBLE
        :type: integer(chfl_property_kind)

        Kind used for ``real(real64)`` (``double`` in C++) values.

    .. f:variable:: CHFL_PROPERTY_STRING
        :type: integer(chfl_property_kind)

        Kind used for ``string`` (``std::string`` in C++) values.

    .. f:variable:: CHFL_PROPERTY_VECTOR3D
        :type: integer(chfl_property_kind)

        Kind used for ``real(real64), dimension(3)`` (``vector3d`` in C++)
        values.

    :return integer(chfl_property_kind):
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_property%bool([status])

    Get the logical value stored in this property. If the property do not store
    a logical value, ``status`` will be :f:var:`CHFL_PROPERTY_ERROR`.

    :return logical:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_property%double([status])

    Get the real value stored in this property. If the property do not store
    a real value, ``status`` will be :f:var:`CHFL_PROPERTY_ERROR`.

    :return real [real64]:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_property%string([status])

    Get the string stored in this property. If the property do not store a
    string, ``status`` will be :f:var:`CHFL_PROPERTY_ERROR`. If the string is
    longer than :f:var:`CHFL_STRING_LENGTH`, it will be truncated.

    :return character(len=CHFL_STRING_LENGTH):
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_property%vector3d([status])

    Get the 3 dimensional array value stored in this property. If the property
    do not store an array, ``status`` will be :f:var:`CHFL_PROPERTY_ERROR`.

    :return real [dimension(3), kind(real64)]:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_property%free()

    Destroy a property, and free the associated memory
