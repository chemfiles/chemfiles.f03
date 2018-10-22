# - Check if the source code provided in the SOURCE argument compiles.
# check_fortran_source_compiles(SOURCE VAR)
# - macro which checks if the source code compiles
#  SOURCE   - source code to try to compile
#  VAR      - variable to store whether the source code compiled
#
# The following variables may be set before calling this macro to
# modify the way the check is run:
#
#  CMAKE_REQUIRED_FLAGS = string of compile command line flags
#  CMAKE_REQUIRED_DEFINITIONS = list of macros to define (-DFOO=bar)
#  CMAKE_REQUIRED_INCLUDES = list of include directories
#  CMAKE_REQUIRED_LIBRARIES = list of libraries to link
macro(check_fortran_source_compiles SOURCE VAR)
    set(MACRO_CHECK_FUNCTION_DEFINITIONS "-D${VAR} ${CMAKE_REQUIRED_FLAGS}")
    if(CMAKE_REQUIRED_LIBRARIES)
        set(CHECK_FORTRAN_SOURCE_COMPILES_ADD_LIBRARIES "-DLINK_LIBRARIES:STRING=${CMAKE_REQUIRED_LIBRARIES}")
    else()
        set(CHECK_FORTRAN_SOURCE_COMPILES_ADD_LIBRARIES)
    endif()

    if(CMAKE_REQUIRED_INCLUDES)
        set(CHECK_FORTRAN_SOURCE_COMPILES_ADD_INCLUDES "-DINCLUDE_DIRECTORIES:STRING=${CMAKE_REQUIRED_INCLUDES}")
    else()
        set(CHECK_FORTRAN_SOURCE_COMPILES_ADD_INCLUDES)
    endif()
    file(WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/src.f90" "${SOURCE}\n")

    message(STATUS "Performing Test ${VAR}")
    try_compile(${VAR}
        ${CMAKE_BINARY_DIR}
        ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/src.f90
        COMPILE_DEFINITIONS ${CMAKE_REQUIRED_DEFINITIONS}
        CMAKE_FLAGS -DCOMPILE_DEFINITIONS:STRING=${MACRO_CHECK_FUNCTION_DEFINITIONS}
        "${CHECK_FORTRAN_SOURCE_COMPILES_ADD_LIBRARIES}"
        "${CHECK_FORTRAN_SOURCE_COMPILES_ADD_INCLUDES}"
        OUTPUT_VARIABLE OUTPUT
    )

    if(${VAR})
        set(${VAR} 1 CACHE INTERNAL "Test ${VAR}")
        message(STATUS "Performing Test ${VAR} - Success")
        file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
            "Performing Fortran SOURCE FILE Test ${VAR} succeded with the following output:\n"
            "${OUTPUT}\n"
            "Source file was:\n${SOURCE}\n"
        )
    else()
        message(STATUS "Performing Test ${VAR} - Failed")
        set(${VAR} 0 CACHE INTERNAL "Test ${VAR}")
        file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
            "Performing Fortran SOURCE FILE Test ${VAR} failed with the following output:\n"
            "${OUTPUT}\n"
            "Source file was:\n${SOURCE}\n"
        )
    endif()
endmacro()

# - Check whether the Fortan compiler supports a given flag.
# check_fortran_compiler_flag(<flag> <var>)
#  <flag> - the compiler flag
#  <var>  - variable to store the result
macro(check_fortran_compiler_flag _flag_ _result_)
    # Skip test if we already did it
    if(NOT DEFINED ${_result_})
        set(SAFE_CMAKE_REQUIRED_DEFINITIONS "${CMAKE_REQUIRED_DEFINITIONS}")
        set(CMAKE_REQUIRED_DEFINITIONS "${_flag_}")
        check_fortran_source_compiles("end" ${_result_}
            # Some compilers do not fail with a bad flag
            FAIL_REGEX "unrecognized .*option"                     # GNU
            FAIL_REGEX "ignoring unknown option"                   # MSVC
            FAIL_REGEX "[Uu]nknown option"                         # HP
            FAIL_REGEX "[Ww]arning: [Oo]ption"                     # SunPro
            FAIL_REGEX "command option .* is not recognized"       # XL
        )
        set(CMAKE_REQUIRED_DEFINITIONS "${SAFE_CMAKE_REQUIRED_DEFINITIONS}")
    endif()
endmacro()
