Miscelaneous functions
======================

.. f:function:: chfl_version()

    Get the version of the Chemfiles library.

    :return character(len=CHFL_STRING_LENGTH):

.. f:variable:: CHFL_STRING_LENGTH
    :type: integer

    Chemfiles uses fixed length strings, containing at most
    ``CHFL_STRING_LENGTH`` characters. If you need longer strings than the
    default (1024), you will need to edit the corresponding source code.

Error handling
--------------

.. f:function:: chfl_last_error()

    Get the last error message emmited by Chemfiles.

    :return character(len=CHFL_STRING_LENGTH):

.. f:subroutine:: chfl_clear_errors([status])

    Clear the last error message emmited by Chemfiles.

    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

All chemfiles functions have an optional ``status`` argument which is an integer
of kind ``chfl_status``, and can take the following values:

.. f:variable:: chfl_status
    :type: integer

    Kind parameter for subroutine and function optional integer status.

.. f:variable:: CHFL_SUCCESS
    :type: integer(chfl_status)

    Status for successful operations.

.. f:variable:: CHFL_MEMORY_ERROR
    :type: integer(chfl_status)

    Status code for error concerning memory: out of memory, wrong size for
    pre-allocated buffers, *etc.*

.. f:variable:: CHFL_FILE_ERROR
    :type: integer(chfl_status)

    Status code for error concerning files: the file do not exist, the user
    does not have rights to open it, *etc.*

.. f:variable:: CHFL_FORMAT_ERROR
    :type: integer(chfl_status)

    Status code for error in file formating, i.e. for invalid files.

.. f:variable:: CHFL_SELECTION_ERROR
    :type: integer(chfl_status)

    Status code for invalid selection strings.

.. f:variable:: CHFL_CONFIGURATION_ERROR
    :type: integer(chfl_status)

    Status code for configuration files errors.

.. f:variable:: CHFL_OUT_OF_BOUNDS
    :type: integer(chfl_status)

    Status code for out of bounds indexing.

.. f:variable:: CHFL_PROPERTY_ERROR
    :type: integer(chfl_status)

    Status code for errors related to properties.

.. f:variable:: CHFL_GENERIC_ERROR
    :type: integer(chfl_status)

    Status code for any other error from Chemfiles.

.. f:variable:: CHFL_CXX_ERROR
    :type: integer(chfl_status)

    Status code for error in the C++ standard library.

Warnings
--------

.. f:subroutine:: chfl_set_warning_callback(callback, [status])

    Chemfiles sends warning on various events, for example invalid files or
    errors in the API usage. By default they are printed to the standard error
    stream, but you can redirect them by setting a callback to be called on each
    event with the event message. This function set the callback for all warning
    events.

    :parameter procedure(chfl_warning_callback) callback: warning callback
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.

.. f:subroutine:: chfl_warning_callback(message)

    Interface for the warning callback to be used with
    ``chfl_set_warning_callback``.

    :parameter string message: The warning message

Configuration files
-------------------

.. f:subroutine:: chfl_add_configuration(path, [status])

    Read `configuration`_ data from the file at ``path``.

    By default, chemfiles reads configuration from any file named
    ``.chemfiles.toml`` or ``chemfiles.toml`` in the current directory or any
    parent directory. This function can be used to add data from another
    configuration file.

    This function will fail if there is no file at path, or if the file is
    incorectly formatted. Data from the new configuration file will overwrite
    any existing data.

    :argument character(len=\*) path: the new configuration file path
    :optional integer(chfl_status) status: status code of the operation. If it
        is not :f:var:`CHFL_SUCCESS`, use :f:func:`chfl_last_error` to learn
        more about the error.


.. _configuration: http://chemfiles.org/chemfiles/latest/configuration.html#configuration
