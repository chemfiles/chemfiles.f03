``chfl_cell`` type
------------------

.. f:currentmodule:: chfl_cell

.. f:type:: chfl_cell

    A :f:type:`chfl_cell` represent the box containing the atoms, and its
    periodicity. An unit cell is fully represented by three lengths (a, b, c);
    and three angles (alpha, beta, gamma). The angles are stored in degrees, and
    the lengths in Angstroms.

    :field subroutine init: :f:func:`chfl_cell%init`
    :field subroutine copy: :f:func:`chfl_cell%copy`
    :field subroutine from_matrix: :f:func:`chfl_cell%from_matrix`
    :field function lengths: :f:func:`chfl_cell%lengths`
    :field subroutine set_lengths: :f:func:`chfl_cell%set_lengths`
    :field function angles: :f:func:`chfl_cell%angles`
    :field subroutine set_angles: :f:func:`chfl_cell%set_angles`
    :field function matrix: :f:func:`chfl_cell%matrix`
    :field function shape: :f:func:`chfl_cell%shape`
    :field subroutine set_shape: :f:func:`chfl_cell%set_shape`
    :field function volume: :f:func:`chfl_cell%volume`
    :field subroutine wrap: :f:func:`chfl_cell%wrap`
    :field subroutine free: :f:func:`chfl_cell%free`


.. f:subroutine:: chfl_cell%init(lengths, [angles, status])

    Initialize this unit cell with an unit cell having the given ``lengths`` and
    optional ``angles``. If all angles are 90°, the unit cell shape is
    :f:var:`CHFL_CELL_ORTHORHOMBIC`, else it is :f:var:`CHFL_CELL_TRICLINIC`.

    This subroutine allocate memory which must be released with
    :f:func:`chfl_cell%free`.

    :argument real lengths(3): cell lengths, in angstroms
    :optional real angles(3): cell angles, in degrees
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_cell%copy(cell, [status])

    Initialize this unit cell with a copy of ``cell``. This subroutine allocate
    memory which must be released with :f:func:`chfl_cell%free`.

    :argument type(chfl_cell) cell: cell to copy
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_cell%from_matrix(matrix, [status])

    Initialize this unit cell from it's matricial representation, i.e the matrix
    containing the three cell vectors. This subroutine allocate
    memory which must be released with :f:func:`chfl_cell%free`.

    :argument real(kind=real64) matrix(3, 3): matrix representation of the unit cell
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_cell%volume([status])

    Get the volume of the unit cell in angstroms cubes.

    :return real:
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_cell%lengths([status])

    Get the unit cell lengths in angstroms.

    :return real(3):
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_cell%set_lengths(lengths, [status])

    Set the unit cell lengths to ``lengths``.

    :argument real lengths(3): new cell lengths, in angstroms
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_cell%angles([status])

    Get the unit cell angles in degrees.

    :return real(3):
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_cell%set_angles(alpha, beta, gamma, [status])

    Set the cell angles to ``angles``. Trying to set cell angles on a cell which
    is not triclinic (does not have the :f:var:`CHFL_CELL_TRICLINIC` shape) is
    an error.

    :argument real angles(3): new cell angles, in degrees
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_cell%matrix([status])

    Get the unit cell matricial representation.

    The unit cell representation is obtained by aligning the a vector along the
    *x* axis and putting the b vector in the *xy* plane. This make the matrix
    an upper triangular matrix:

    .. code-block:: sh

        | a_x b_x c_x |
        |  0  b_y c_y |
        |  0   0  c_z |

    :return real(3, 3):
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:function:: chfl_cell%shape([status])

    Get the unit cell shape.

    :return integer(chfl_cellshape):
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

    The cell shapes are integers which ``kind`` is the :f:var:`chfl_cellshape`
    parameter.

    .. f:variable:: chfl_cellshape
        :type: integer

        Integer kind parameter for representing unit cel shape.

    .. f:variable:: CHFL_CELL_ORTHORHOMBIC
        :type: integer(chfl_cellshape)

        Cell shape for cell where the three angles are 90°

    .. f:variable:: CHFL_CELL_TRICLINIC
        :type: integer(chfl_cellshape)

        Cell shape for cell where the three angles may not be 90°

    .. f:variable:: CHFL_CELL_INFINITE
        :type: integer(chfl_cellshape)

        Cell type when there is no periodic boundary conditions

.. f:subroutine:: chfl_cell%set_shape(shape, [status])

    Set the unit cell shape to ``shape``

    :argument integer(chfl_cellshape) shape: the new shape of the cell
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_cell%wrap(vector, [status])

    Wrap a ``vector`` in this unit cell so that all its components are beteen
    ``-L/2`` and ``L/2``.

    :argument real vector(3): vector to wrap in the cell
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_cell%free()

    Destroy an unit cell, and free the associated memory
