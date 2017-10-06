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
