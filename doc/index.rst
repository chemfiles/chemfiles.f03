Fortran interface to chemfiles
==============================

This is the documentation for the Fortran 2003 interface to the `chemfiles`_
library.  This Fortran interface to chemfiles is built in an object-oriented
fashion, using the C interface of chemfiles and the ``iso_c_binding`` intrisic
module.

All the functionalities are in the ``chemfiles`` module, which should be used in
all the programs using chemfiles. The ``iso_fortran_env`` instrisic module can
also be usefull to set the kind of real and doubles where needed.

Installation
^^^^^^^^^^^^

The fortran interface have the same dependencies as the main C++ library, which
are documented `here <chemfiles-deps_>`_. It also need a modern fortran
compiler. ``gfortran`` (4.9) and ``ifort`` (14) are known to work. If you manage
to build it with another compiler, please tell me so that I can add it here.


.. code-block:: sh

    # Get the sources:
    git clone --recursive https://github.com/chemfiles/chemfiles.f03
    cd chemfiles.f03
    mkdir build && cd build
    # Configure for installation in <PREFIX>
    cmake -DCMAKE_INSTALL_PREFIX=<PREFIX> -DCHFL_FORTRAN_BUILD_TESTS=ON ..
    # Build
    make
    # run tests (optional, but recommended)
    ctest
    # install to <PREFIX>
    make install

Other options are available and documented with the `main C++ library
<chemfiles-opts_>`_.

Usage
^^^^^

Once you have installed chemfiles to a given ``<PREFIX>``, you can use it in
your code with the ``use chemfiles`` directive:

.. code-block:: fortran

    program test
        use chemfiles
        implicit none

        ! some code
    end program

Then you can compile this file with your favorite Fortran compiler, here
assuming gfortran or intel ifort:

.. code-block:: sh

    # replace <FC> with your compiler: gfortran, ifort
    <FC> -c main.f90 -o main.o -I<PREFIX>/include

The ``-I<PREFIX>/include`` might not be needed, depending on where you
installed chemfiles. This must be the path to the ``chemfiles.mod`` file.

Linking your program into a final executable is a bit trickier, as Chemfiles is
a C++ library, and both C++ and Fortran have a non-zero runtime (libstc++/libc++
for C++ and libgfortran/libifort for Fortran). This means that you need to link
both runtimes in the final executable.

The easiest way to do this is to build chemfiles as a shared library, by
configuring the code with ``cmake -DBUILD_SHARED_LIBS=ON ..``. Then, you can
link you final executable with:

.. code-block:: sh

    <FC> main.o -o main -lchemfilesf03 -lchemfiles

The order of the libraries on the command line is important (``-lchemfilesf03
-lchemfiles`` is not the same as ``-lchemfiles -lchemfilesf03``).

If you want to use static libraries, then you will need to explicitly link the
C++ and Fortran runtime manually. On OS X, the C++ runtime is linked with
``-lc++`` and on Linux you need ``-lstdc++``. You can also link gfortran fortran
runtime with ``-lgfortran``. For intel ifort you should use ifort as a linker
and whatever C++ library is needed.

User documentation
^^^^^^^^^^^^^^^^^^

This section contains example of how to use chemfiles, and the complete
interface reference for all the types and subroutines in chemfiles.

.. toctree::
   :maxdepth: 3

   examples
   reference/index


.. _chemfiles: https://github.com/chemfiles/chemfiles
.. _chemfiles-deps: http://chemfiles.org/chemfiles/latest/installation.html#core-library-dependencies
.. _chemfiles-opts: http://chemfiles.org/chemfiles/latest/installation.html#build-steps
