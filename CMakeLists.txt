cmake_minimum_required(VERSION 2.8.11)

project(chemfilesf03 Fortran CXX)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")
if(${CMAKE_VERSION} VERSION_GREATER 3.0.0)
    cmake_policy(SET CMP0042 NEW)
endif()

option(CHFL_FORTRAN_BUILD_TESTS "Build fortran unit tests." OFF)

include(FortanCompilerFlags)
# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'release' as none was specified.")
  set(CMAKE_BUILD_TYPE "release" CACHE STRING "Choose the type of build." FORCE)
endif()

# Check that the git submodule exists
if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/chemfiles/CMakeLists.txt")
    message(FATAL_ERROR
        "The git submodule for chemfiles is not initalized.\n"
        "Please run `git submodule update --init`"
    )
endif()
add_subdirectory(chemfiles)

set(sources
    src/ffi.f90
    src/strings.f90
    src/atom.f90
    src/cell.f90
    src/frame.f90
    src/property.f90
    src/residue.f90
    src/selection.f90
    src/topology.f90
    src/trajectory.f90
    src/chemfiles.f90
)

add_library(chemfilesf03 ${sources})
target_link_libraries(chemfilesf03 chemfiles ${CMAKE_CXX_IMPLICIT_LINK_LIBRARIES})
set_property(TARGET chemfilesf03 PROPERTY VERSION ${CHEMFILES_VERSION})
set_property(TARGET chemfilesf03 PROPERTY SOVERSION ${CHEMFILES_VERSION})

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/chemfiles.mod DESTINATION include)
install(TARGETS chemfilesf03 DESTINATION lib)

CHECK_Fortran_COMPILER_FLAG("-Wno-compare-reals" FC_SUPPORTS_NO_COMPARE_REAL)

if(${CHFL_CODE_COVERAGE})
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fprofile-arcs -ftest-coverage -O0")
    set(CMAKE_BUILD_TYPE "debug" CACHE STRING "Choose the type of build." FORCE)
    set(COVERAGE_LIBRARIES gcov)
endif()

function(chfl_fortran_test _file_)
    get_filename_component(_name_ ${_file_} NAME_WE)
    set(_name_ "fortran-${_name_}")
    add_executable(${_name_} ${_file_})
    target_link_libraries(${_name_} chemfilesf03 ${COVERAGE_LIBRARIES})
    if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
        # ifort needs to link to 'for_main.o'
        set_target_properties(${_name_} PROPERTIES LINKER_LANGUAGE Fortran)
    else()
        set_target_properties(${_name_} PROPERTIES LINKER_LANGUAGE CXX)
    endif()
    add_test(NAME ${_name_}
        COMMAND ${_name_}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
    )
    if(FC_SUPPORTS_NO_COMPARE_REAL)
        # We are comparing reals in the tests
        set_target_properties(${_name_} PROPERTIES COMPILE_FLAGS "-Wno-compare-reals")
    endif()
endfunction()

function(chfl_fortran_example _file_)
    get_filename_component(_name_ ${_file_} NAME_WE)
    set(_name_ example-f90-${_name_})
    add_executable(${_name_} ${_file_})
    target_link_libraries(${_name_} chemfilesf03 ${COVERAGE_LIBRARIES})
    if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
        # ifort needs to link to 'for_main.o'
        set_target_properties(${_name_} PROPERTIES LINKER_LANGUAGE Fortran)
    else()
        set_target_properties(${_name_} PROPERTIES LINKER_LANGUAGE CXX)
    endif()
endfunction()

enable_testing()
if(${CHFL_FORTRAN_BUILD_TESTS})
    file(GLOB f_test_files ${CMAKE_CURRENT_SOURCE_DIR}/tests/*.F90)

    foreach(test_file IN LISTS f_test_files)
        chfl_fortran_test(${test_file})
    endforeach(test_file)

    file(GLOB examples examples/*.f90)
    foreach(example IN LISTS examples)
        chfl_fortran_example(${example})
    endforeach()
endif()
