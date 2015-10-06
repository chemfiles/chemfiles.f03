cmake_minimum_required(VERSION 2.8.12)
project(chemharpf Fortran)
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

add_subdirectory(chemharp)

file(GLOB sources src/*.f90)
add_library(chemharpf ${sources})
target_link_libraries(chemharpf chemharp)
set_property(TARGET chemharpf PROPERTY VERSION ${CHRP_VERSION})
set_property(TARGET chemharpf PROPERTY SOVERSION ${CHRP_VERSION})

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/chemharp.mod DESTINATION include)
install(TARGETS chemharpf DESTINATION lib)

function(chrp_fortran_test _file_)
    get_filename_component(_name_ ${_file_} NAME_WE)
    set(_name_ "f-${_name_}")
    add_executable(${_name_} ${_file_})
    target_link_libraries(${_name_} chemharp chemharpf fortran-testing)
    add_test(NAME ${_name_} COMMAND ${_name_} "${PROJECT_SOURCE_DIR}/Chemharp/tests/data")
endfunction()

function(chrp_fortran_example _file_)
    get_filename_component(_name_ ${_file_} NAME_WE)
    add_executable(example-f90-${_name_} ${_file_})
    target_link_libraries(example-f90-${_name_} chemharp chemharpf)
endfunction()

if(${BUILD_TESTS})
    enable_testing()
    file(GLOB f_test_files ${CMAKE_CURRENT_SOURCE_DIR}/tests/*.f90)
    add_library(fortran-testing STATIC ${CMAKE_CURRENT_SOURCE_DIR}/tests/mod/testing.f90)

    foreach(test_file IN LISTS f_test_files)
        chrp_fortran_test(${test_file})
    endforeach(test_file)

    include_directories(${PROJECT_BINARY_DIR})
    file(GLOB examples examples/*.f90)
    foreach(example IN LISTS examples)
        chrp_fortran_example(${example})
    endforeach()
endif()
