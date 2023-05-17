Tutorials
=========

This section present some hand-on tutorials to the chemfiles Fortran API. These
tutorial do not check for errors. When using chemfiles you might want to check
for error using the ``status`` parameter that all functions take.

All the code here is under the `CC-0 Universal Licence`_ which means that you
are free to do whatever you want with it (*i.e.* it is Public Domain code)

.. _CC-0 Universal Licence: https://creativecommons.org/publicdomain/zero/1.0/

Read a single frame
-------------------

In this tutorials we will read a frame from a trajectory, and print the indexes
of all the atom in the half-space ``x < 5``.

We start by ``use``ing the modules we will need: ``chemfiles``, and
``iso_fortran_env``.  ``iso_fortran_env`` provides fixed width types that are
very usefull when using a C++ library like chemfiles from Fortran. Here, we only
need 64-bits integers and real.

.. literalinclude:: ../examples/indexes.f90
   :language: fortran
   :lines: 5-7
   :dedent: 4

We can then declare all the variables we will need. All chemfiles specific types
are user defined type that should be declared with ``type(chfl_xxx)``. In this
example we will also need a few other values: an array for positions and an
array for the indexes of atoms with ``x < 5``. The positions array is a pointer
because we will not allocate memory for it. Instead we will directly access some
memory inside chemfiles internal data structures.

.. literalinclude:: ../examples/indexes.f90
   :language: fortran
   :lines: 9-14
   :dedent: 4

Then we open a :f:type:`chfl_trajectory` in read (``'r'``) mode and read the
first frame.  We need to initialize memory for the :f:type:`chfl_frame` before
calling :f:func:`chfl_trajectory%read`.

.. literalinclude:: ../examples/indexes.f90
   :language: fortran
   :lines: 16-18
   :dedent: 4

We can now and get the positions of the atoms and the number of atoms in the
frame with the :f:func:`chfl_frame%positions` subroutine. This function will set
natoms to the number of atoms in the frame.

.. literalinclude:: ../examples/indexes.f90
   :language: fortran
   :lines: 20
   :dedent: 4

Knowning the total number of atoms in the frame, we can allocate memory to store
the indices of the atoms with ``x < 5``:

.. literalinclude:: ../examples/indexes.f90
   :language: fortran
   :lines: 21
   :dedent: 4

Iterating through the atoms in the frame, we get the ones matching our
condition. We need to track the number of ``matched`` atoms to know where to add
them in the ``less_than_five`` array.

.. literalinclude:: ../examples/indexes.f90
   :language: fortran
   :lines: 23-29
   :dedent: 4

At the end we can print our results

.. literalinclude:: ../examples/indexes.f90
   :language: fortran
   :lines: 31-34
   :dedent: 4

And free all allocated memory. We don't need to free ``positions``, as it points
into memory allocated and controlled by the frame.

.. literalinclude:: ../examples/indexes.f90
   :language: fortran
   :lines: 36-38
   :dedent: 4

.. htmlhidden::
    :toggle: Click here to see the whole program
    :before-not-html: The whole code looks like this

    .. literalinclude:: ../examples/indexes.f90
       :language: fortran
       :lines: 4-

For more information about reading frame in a trajectory, see the following
functions:

- :f:func:`chfl_trajectory%nsteps` to know when to stop reading
- :f:func:`chfl_trajectory%read_step` to directlty read a given step.
- :f:func:`chfl_trajectory%set_cell` and :f:func:`chfl_trajectory%set_topology`
  to specify an unit cell or a topology for all frames in a trajectory.

Generating a structure
----------------------

Now that we know how to read frames from a trajectory, let's try to create a new
structure and write it to a file. As previsouly, we start by ``use``ing the
chemfiles and ``iso_fortran_env`` modules:

.. literalinclude:: ../examples/generate.f90
   :language: fortran
   :lines: 5-7
   :dedent: 4

Again, we declare everything we will need: a topology, some atoms, a frame, a
cell and a trajectory.

.. literalinclude:: ../examples/generate.f90
   :language: fortran
   :lines: 9-15
   :dedent: 4

Everything starts with a :f:type:`chfl_topology`. This is the type that defines
the atoms and the connectivity in a system. Here, we add three
:f:type:`chfl_atom` to the topology.

.. literalinclude:: ../examples/generate.f90
   :language: fortran
   :lines: 17-24
   :dedent: 4

Then we can add bonds between these atoms. It is worth noting two things here:
first the atomic number are converted to ``int64`` before passing them to the
function (one could also change the default size of integers with a global
compiler flag). Second, the atomic numbers starts at 0, not 1. This might be
source of confusion when using chemfiles, as indexes usually start at 1 in
Fortran;

.. literalinclude:: ../examples/generate.f90
   :language: fortran
   :lines: 26-27
   :dedent: 4

We can then create a :f:type:`chfl_frame` and set its topology. We free the
topology right after, because we no longer need it.

.. literalinclude:: ../examples/generate.f90
   :language: fortran
   :lines: 29-32
   :dedent: 4

Once we set the topology, we can set the positions of the atoms. Notice how the
indexes starts at 1 here, as we are using a standard fortran array.

.. literalinclude:: ../examples/generate.f90
   :language: fortran
   :lines: 34-37
   :dedent: 4

Another possibility is to directly add atoms and bonds to the frame. Here we
define a second molecule representing carbon dioxyde.

.. literalinclude:: ../examples/generate.f90
   :language: fortran
   :lines: 39-43
   :dedent: 4

Finally, we can set the :f:type:`chfl_cell` associated with this frame. We
also free the cell memory, as it is no longer needed.

.. literalinclude:: ../examples/generate.f90
   :language: fortran
   :lines: 45-47
   :dedent: 4

Now that our frame is constructed, it is time to write it to a file. For that,
we open a trajectory in write (``'w'``) mode, and write to it.

.. literalinclude:: ../examples/generate.f90
   :language: fortran
   :lines: 49-51
   :dedent: 4

And free all remaining memory with the right functions.

.. literalinclude:: ../examples/generate.f90
   :language: fortran
   :lines: 53-56
   :dedent: 4

.. htmlhidden::
    :toggle: Click here to see the whole program
    :before-not-html: Wrapping everything up, the whole code looks like this:

    .. literalinclude:: ../examples/generate.f90
       :language: fortran
       :lines: 4-

Using selections
----------------

Now that we know how to read and write frame from trajectories, how about we do
a bit a filtering? In this tutorial, we will read all the frames from a file,
and use `selections`_ to filter which atoms we will write back to another file.
This example will also show how chemfiles can be used to convert from a file
format to another one.

.. _selections: http://chemfiles.org/chemfiles/latest/selections.html

We start by declaring all the variables we need: two trajectories, a frame, a
selection and an array of matches.

.. literalinclude:: ../examples/select.f90
   :language: fortran
   :lines: 9-15
   :dedent: 4

Then we can open the two trajectories we need

.. literalinclude:: ../examples/select.f90
   :language: fortran
   :lines: 17-18
   :dedent: 4

We create a :f:type:`chfl_frame` and a :f:type:`chfl_selection` object to
filter the atoms we want to keep.

.. literalinclude:: ../examples/select.f90
   :language: fortran
   :lines: 19-20
   :dedent: 4

Then we get the number of steps in the trajectory

.. literalinclude:: ../examples/select.f90
   :language: fortran
   :lines: 22-23
   :dedent: 4

And iterate over the frames in the trajectory

.. literalinclude:: ../examples/select.f90
   :language: fortran
   :lines: 25-26
   :dedent: 4

From here, we need to use the selection to get the atoms we want to remove. This
is a two steps process: first we evaluate the selection and get the number of
matches

.. literalinclude:: ../examples/select.f90
   :language: fortran
   :lines: 28-29
   :dedent: 8

Second we allocate some memory and get all the matches (represented as
:f:type:`chfl_match`):

.. literalinclude:: ../examples/select.f90
   :language: fortran
   :lines: 30-31
   :dedent: 8

We can get the index of atoms in a `to_remove` array

.. literalinclude:: ../examples/select.f90
   :language: fortran
   :lines: 33-36
   :dedent: 8

In order to remove the atoms from the frame, we need to sort ``to_remove`` in
descending order: removing the atom at index i will shift the index of all the
atoms after i. So we need start from the end and work toward the start of the
frame.

.. literalinclude:: ../examples/select.f90
   :language: fortran
   :lines: 38-41
   :dedent: 8

Finally, we can write the cleaned frame to the output file, and free the memory
we allocated:

.. literalinclude:: ../examples/select.f90
   :language: fortran
   :lines: 43-44
   :dedent: 8

The ``sort`` function we used to sort the matches is defined as follow:

.. literalinclude:: ../examples/select.f90
   :language: fortran
   :lines: 53-70
   :dedent: 4

.. htmlhidden::
    :toggle: Click here to see the whole program
    :before-not-html: The whole program look like this:

    .. literalinclude:: ../examples/select.f90
       :language: fortran
       :lines: 4-
