cmake_minimum_required(VERSION 2.8.11)

project(chemfilesf03 Fortran CXX)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")
if(${CMAKE_VERSION} VERSION_GREATER 3.0.0)
    cmake_policy(SET CMP0042 NEW)
endif()

include(FortanCompilerFlags)
# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'release' as none was specified.")
  set(CMAKE_BUILD_TYPE "release" CACHE STRING "Choose the type of build." FORCE)
endif()

add_subdirectory(chemfiles)

file(GLOB sources src/*.f90)
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
    target_link_libraries(${_name_} chemfilesf03 fortran-testing ${COVERAGE_LIBRARIES})
    add_test(NAME ${_name_} COMMAND ${_name_} "${PROJECT_BINARY_DIR}/chemfiles/tests/data")
    if(FC_SUPPORTS_NO_COMPARE_REAL)
        # We are comparing reals in the tests
        set_target_properties(${_name_} PROPERTIES COMPILE_FLAGS "-Wno-compare-reals")
    endif()
endfunction()

function(chfl_fortran_example _file_)
    get_filename_component(_name_ ${_file_} NAME_WE)
    add_executable(example-f90-${_name_} ${_file_})
    target_link_libraries(example-f90-${_name_} chemfilesf03 ${COVERAGE_LIBRARIES})
endfunction()

if(${CHFL_BUILD_TESTS})
    enable_testing()
    file(GLOB f_test_files ${CMAKE_CURRENT_SOURCE_DIR}/tests/*.f90)
    add_library(fortran-testing STATIC ${CMAKE_CURRENT_SOURCE_DIR}/tests/mod/testing.f90)

    foreach(test_file IN LISTS f_test_files)
        chfl_fortran_test(${test_file})
    endforeach(test_file)

    include_directories(${PROJECT_BINARY_DIR})
    file(GLOB examples examples/*.f90)
    foreach(example IN LISTS examples)
        chfl_fortran_example(${example})
    endforeach()
endif()
