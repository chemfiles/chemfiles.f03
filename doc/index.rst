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
    mkdir build
    # Configure for installation in PREFIX
    cmake -DCMAKE_INSTALL_PREFIX=<PREFIX> ..
    # Build
    make
    # run tests (optional, but recommended)
    ctest
    # install to `PREFIX`
    make install

Other options are available and documented with the `main C++ library
<chemfiles-opts_>`_.

Usage
^^^^^

To compile your own code using ``chemfiles``, you need to ``use chemfiles`` in
your program, and add the ``chemfilesf03`` and ``chemfiles`` libraries to the
linker arguments. On GNU/Linux and OS X, this is done by the ``-lchemfilesf03
-lchemfiles`` flags. If you want to compile ``main.f90`` with gfortran, you need
to run

.. code-block:: sh

    gfortran main.f90 -o main -lchemfilesf03 -lchemfiles

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
