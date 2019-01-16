#include "check.inc"

program residue_test
    use iso_fortran_env, only: real64
    use chemfiles
    implicit none

    call test_bool()
    call test_double()
    call test_string()
    call test_vector3d()

contains
    subroutine test_bool()
        implicit none
        type(chfl_property) :: property
        integer :: status

        call property%init(.false., status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(property%bool(status=status) .eqv. .false.)
        CHECK(status == CHFL_SUCCESS)

        CHECK(property%kind(status=status) == CHFL_PROPERTY_BOOL)
        CHECK(status == CHFL_SUCCESS)

        call property%free()
        ! Call free twice to check that it works
        call property%free()
    end subroutine

    subroutine test_double()
        implicit none
        type(chfl_property) :: property
        integer :: status

        call property%init(3.4d0, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(property%double(status=status) == 3.4d0)
        CHECK(status == CHFL_SUCCESS)

        CHECK(property%kind(status=status) == CHFL_PROPERTY_DOUBLE)
        CHECK(status == CHFL_SUCCESS)

        call property%free()
    end subroutine

    subroutine test_string()
        implicit none
        type(chfl_property) :: property
        integer :: status

        call property%init("test", status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(property%string(status=status) == 'test')
        CHECK(status == CHFL_SUCCESS)

        CHECK(property%kind(status=status) == CHFL_PROPERTY_STRING)
        CHECK(status == CHFL_SUCCESS)

        call property%free()
    end subroutine

    subroutine test_vector3d()
        implicit none
        type(chfl_property) :: property
        integer :: status

        call property%init([11d0, 22d0, 33d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(all(property%vector3d(status=status) == [11d0, 22d0, 33d0]))
        CHECK(status == CHFL_SUCCESS)

        CHECK(property%kind(status=status) == CHFL_PROPERTY_VECTOR3D)
        CHECK(status == CHFL_SUCCESS)

        call property%free()
    end subroutine
end program
