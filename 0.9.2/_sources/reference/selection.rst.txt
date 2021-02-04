``chfl_selection`` type
------------------------

.. f:type:: chfl_selection

    :f:type:`chfl_selection` allow to get the index of atoms in a
    :f:type:`chfl_frame`, matching a selection language. The matching atoms are
    given in an array of :f:type:`chfl_match`.

    :field subroutine init: :f:func:`chfl_selection%init`
    :field subroutine copy: :f:func:`chfl_selection%copy`
    :field function size: :f:func:`chfl_selection%size`
    :field function string: :f:func:`chfl_selection%string`
    :field subroutine evaluate: :f:func:`chfl_selection%evaluate`
    :field subroutine matches: :f:func:`chfl_selection%matches`
    :field subroutine free: :f:func:`chfl_selection%free`

.. f:type:: chfl_match

    This type contains the matched atoms for a given selection in the ``atoms``
    array. Values in the ``atoms`` array are valid up to the ``size`` of this
    match. If the match size is 2, then ``atom(1)`` and ``atom(2)`` are valid,
    and ``atom(3)`` and ``atom(4)`` contains invalid indexes.

    :field integer size: The size of this match.
    :field integer atoms(4): The index of the matched atoms.

.. f:subroutine:: chfl_selection%init(selection, [status])

    Initialize the selection from the given ``selection`` string. This
    subroutine allocate memory which must be released with
    :f:func:`chfl_selection%free`.

    See the `selection documentation`_ for the selection language specification.

    .. _selection documentation: http://chemfiles.org/chemfiles/latest/selections.html

    :argument character(len=\*) selection: The selection string
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_selection%copy(selection, [status])

    Initialize the selection with a copy of ``selection``. This subroutine
    allocate memory which must be released with :f:func:`chfl_selection%free`.

    The copy does not contains any state, and :f:func:`chfl_selection%evaluate`
    must be called again before using :f:func:`chfl_selection%matches`.

    :argument type(chfl_selection) selection: selection to copy
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_selection%size([status])

    Get the size of the selection. This is the number of atoms we are selecting
    together. This value is 1 for the 'atom' context, 2 for the 'pair' and
    'bond' context, 3 for the 'three' and 'angles' contextes and 4 for the
    'four' and 'dihedral' contextes.

    :returns integer: selection size
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_selection%string([status])

    Get the selection string used to create this selection. If the string is
    longer than :f:var:`CHFL_STRING_LENGTH`, it will be truncated.

    :return character(len=CHFL_STRING_LENGTH): selection string
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_selection%evaluate(frame, count, [status])

    Evaluate the selection for a given ``frame``, and store the number of
    matches in ``count``. Use :f:func:`chfl_selection%matches` to get the
    matches.

    :argument type(chfl_frame) frame: frame to evaluate
    :argument integer count: number of matches for this selection
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.


.. f:subroutine:: chfl_selection%matches(matches, [status])

    Get the matches for the ``selection`` after a call to
    :f:func:`chfl_selection%evalutate`, in the pre-allocated ``matches`` array.

    :argument type(chfl_match) matches [dimension(\:)]: Array of :f:type:`chfl_match`
        of the size given by :f:func:`chfl_selection%evaluate`.
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_selection%free()

    Destroy a selection, and free the associated memory
