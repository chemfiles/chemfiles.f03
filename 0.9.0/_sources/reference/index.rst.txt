.. _fortran-api:

Fortran interface reference
===========================

The ``chemfiles`` module is built around the main types of chemfiles:
:f:type:`chfl_trajectory`, :f:type:`chfl_frame`, :f:type:`chfl_cell`,
:f:type:`chfl_topology`, :f:type:`chfl_residue`, :f:type:`chfl_atom`,
:f:type:`chfl_selection`,  and :f:type:`chfl_property`. For more information
about these types, please see the chemfiles `overview`_.

.. warning::

    Atomic indexes in chemfiles starts at 0, not 1. That means that the first
    atom in a frame have the index 0, not 1. This should be taken in account
    when using chemfiles functions. Fortran arrays returned by function still
    have indexes starting at 1.

    .. code-block :: fortran

        program indexing
            use iso_fortran_env, only: real64
            use chemfiles
            implicit none
            type(chfl_frame)      :: frame
            type(chfl_atom)       :: atom
            real(real64), pointer :: positions(:, :)

            ! Initialize the frame ...

            ! Get the first atom in the frame
            atom = frame%atom(0)
            call atom%free()
            ! Get the second atom in the frame
            atom = frame%atom(1)
            call atom%free()

            positions => frame%positions()
            ! positions is a fortran array, so positions(1, :) now contains
            ! the positions of the first atom, and not positions(0, :)
        end program

.. _overview: http://chemfiles.org/chemfiles/latest/overview.html

Conventions
-----------

All the free functions and types have the ``chfl_`` prefix. Most functions take
an optional ``status`` argument which will indicate the status of the operation.
It should be :f:var:`CHFL_SUCCESS` if everything was OK, and another value in
case of error.

When creating a variable of one of the chemfiles types, the first routine to be
called should be an initialization routine. It can be either the ``init``
routine for default initialization, or another routine documented as
initializing.

These initialization function should only be called once. In order to free the
memory asssociated with any chemfiles variable, the ``free`` subroutine should
be called. After a call the the ``free`` subroutine, the ``init`` subroutine can
be called again whithout any memory leak risk. Not initializing chemfiles
variables will lead to errors and segmentation faults.

.. code-block:: fortran

    type(chfl_cell) :: cell
    type(chfl_frame) :: frame

    ! Initialize the variables
    call cell%init([20, 20, 20])
    call frame%init()

    ! free the memory
    call cell%free()
    call frame%free()

Types and associated subroutines
--------------------------------

.. toctree::
   :maxdepth: 2

   misc
   trajectory
   frame
   cell
   topology
   residue
   atom
   selection
   property
