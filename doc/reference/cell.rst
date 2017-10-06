``chfl_cell`` type
------------------

.. f:currentmodule:: chfl_cell

.. f:type:: chfl_cell

    A :f:type:`chfl_cell` represent the box containing the atoms, and its
    periodicity.

    An unit cell is fully represented by three lengths (a, b, c); and three
    angles (alpha, beta, gamma). The angles are stored in degrees, and the
    lengths in Angstroms.

    The initialization routine for :f:type:`chfl_cell` are
    :f:func:`chfl_cell%init`, :f:func:`chfl_cell%triclinic`,
    :f:func:`chfl_cell%from_frame` and :f:func:`chfl_cell%copy`.

    :field subroutine init: :f:func:`chfl_cell%init`
    :field subroutine triclinic: :f:func:`chfl_cell%triclinic`
    :field subroutine from_frame: :f:func:`chfl_cell%from_frame`
    :field subroutine copy: :f:func:`chfl_cell%copy`
    :field subroutine lengths: :f:func:`chfl_cell%lengths`
    :field subroutine set_lengths: :f:func:`chfl_cell%set_lengths`
    :field subroutine angles: :f:func:`chfl_cell%angles`
    :field subroutine set_angles: :f:func:`chfl_cell%set_angles`
    :field subroutine matrix: :f:func:`chfl_cell%matrix`
    :field subroutine shape: :f:func:`chfl_cell%shape`
    :field subroutine set_shape: :f:func:`chfl_cell%set_shape`
    :field subroutine volume: :f:func:`chfl_cell%volume`
    :field subroutine free: :f:func:`chfl_cell%free`


.. f:subroutine:: chfl_cell%init(lengths, [status])

    Initialize this unit cell with an unit cell having the given ``lengths``.
    The unit cell shape is :f:var:`CHFL_CELL_ORTHORHOMBIC`.

    :argument real lengths(3): cell lengths, in angstroms
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%triclinic(lengths, angles, [status])

    Initialize this unit cell with an unit cell having the given ``lengths`` and
    ``angles``. The unit cell shape is :f:var:`CHFL_CELL_TRICLINIC`.

    :argument real lengths(3): cell lengths, in angstroms
    :argument real angles(3): cell angles, in degrees
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%copy(cell, [status])

    Initialize this unit cell with a copy of ``cell``.

    :argument chfl_cell cell: cell to copy
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%from_frame(frame, [status])

    Initialize this topology with a copy of the :f:type:`chfl_cell` of a frame.

    :argument chfl_frame frame: the frame
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%volume(volume, [status])

    Get the volume of the unit cell in ``volume``.

    :argument real volume: volume of the unit cell
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%lengths(lengths, [status])

    Get the unit cell lengths in ``lengths``.

    :argument real lengths(3): cell lengths, in angstroms
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%set_lengths(lengths, [status])

    Set the unit cell lengths to ``lengths``.

    :argument real lengths(3): new cell lengths, in angstroms
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%angles(angles, [status])

    Get the unit cell angles in ``angles``.

    :argument real angles(3): cell angles, in degrees
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%set_angles(alpha, beta, gamma, [status])

    Set the cell angles to ``angles``. Trying to set cell angles on a cell which
    is not triclinic (does not have the ``CHFL_CELL_TRICLINIC`` shape) is an
    error.

    :argument real angles(3): new cell angles, in degrees
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%matrix(matrix, [status])

    Get the unit cell matricial representation in ``matrix``.

    The unit cell representation is obtained by aligning the a vector along the
    *x* axis and putting the b vector in the *xy* plane. This make the matrix
    an upper triangular matrix:

    .. code-block:: sh

        | a_x b_x c_x |
        |  0  b_y c_y |
        |  0   0  c_z |


    :argument real matrix(3, 3): unit cell matrix
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%shape(shape, [status])

    Get the unit cell shape in ``shape``.

    :argument integer type [kind=chfl_cell_shape_t]: the shape of the cell
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

    The cell shapes are integers which ``kind`` is the ``chfl_cell_shape_t``
    parameter:

    .. f:variable:: integer(chfl_cell_shape_t) :: CHFL_CELL_ORTHORHOMBIC

        The three angles are 90°

    .. f:variable:: integer(chfl_cell_shape_t) :: CHFL_CELL_TRICLINIC

        The three angles may not be 90°

    .. f:variable:: integer(chfl_cell_shape_t) :: CHFL_CELL_INFINITE

        Cell type when there is no periodic boundary conditions

.. f:subroutine:: chfl_cell%set_shape(shape, [status])

    Set the unit cell shape to ``shape``

    :argument integer type [kind=chfl_cell_shape_t]: the new type of the cell
    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_cell%free([status])

    Destroy an unit cell, and free the associated memory

    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.
