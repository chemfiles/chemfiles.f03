include(CheckFortranCompilerFlag)
include(CheckFortranCompilerFlag)

macro(set_fortran_debug_flag_if_possible _flag_)
    CHECK_Fortran_COMPILER_FLAG("${_flag_}" FC_SUPPORTS${_flag_})
    if(FC_SUPPORTS${_flag_})
        set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${_flag_}")
    endif()
endmacro()

# gfortran
set_fortran_debug_flag_if_possible("-Wall")
set_fortran_debug_flag_if_possible("-Wextra")
set_fortran_debug_flag_if_possible("-Wimplicit-interface")
set_fortran_debug_flag_if_possible("-Wimplicit-procedure")
set_fortran_debug_flag_if_possible("-Wno-c-binding-type")

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008 -pedantic")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-form")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
endif()
