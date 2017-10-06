Error and logging functions
---------------------------

.. f:function:: chfl_version()

    Get the version of the Chemfiles library.

    :return character [len=*]: chemfiles version

.. f:function:: chfl_last_error()

    Get the last error message emmited by Chemfiles.

    :return character [len=*]: error message for the last error

.. f:subroutine:: chfl_clear_errors([status])

    Clear the last error message emmited by Chemfiles.

    :optional integer status [optional, kind=chfl_status]: status code of the
        operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
        about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_set_warning_callback(callback, [status])

    Chemfiles sends warning on various events, for example invalid files or
    errors in the API usage. By default they are printed to the standard error
    stream, but you can redirect them by setting a callback to be called on each
    event with the event message. This function set the callback for all warning
    events.

    :parameter procedure callback [kind=chfl_warning_callback]: warning callback
    :optional integer status [optional, kind=chfl_status]: status code of the
          operation. If it is not equal to ``CHFL_SUCCESS``, you can learn more
          about the error by using ``chfl_last_error``.

.. f:subroutine:: chfl_warning_callback(message)

    Interface for the warning callback to be used with
    ``chfl_set_warning_callback``.

    :parameter character message [len=*, intent(in)]: The warning message


The optional status argument is an integer of kind ``chfl_status``, which can
take the following values:

.. f:variable:: integer(chfl_status) :: CHFL_SUCCESS

    Status for successful operations.

.. f:variable:: integer(chfl_status) :: CHFL_MEMORY_ERROR

    Status code for error concerning memory: out of memory, wrong size for
    pre-allocated buffers, *etc.*

.. f:variable:: integer(chfl_status) :: CHFL_FILE_ERROR

    Status code for error concerning files: the file do not exist, the user
    does not have rights to open it, *etc.*

.. f:variable:: integer(chfl_status) :: CHFL_FORMAT_ERROR

    Status code for error in file formating, i.e. for invalid files.

.. f:variable:: integer(chfl_status) :: CHFL_SELECTION_ERROR

    Status code for invalid selection strings.

.. f:variable:: integer(chfl_status) :: CHFL_GENERIC_ERROR

    Status code for any other error from Chemfiles.

.. f:variable:: integer(chfl_status) :: CHFL_CXX_ERROR

    Status code for error in the C++ standard library.
