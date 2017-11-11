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
        integer(chfl_property_kind) :: kind
        logical(1) :: value
        integer :: status

        call property%bool(logical(.false., 1), status=status)
        CHECK(status == CHFL_SUCCESS)

        call property%get_bool(value, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(value .eqv. .false.)

        call property%get_kind(kind, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(kind == CHFL_PROPERTY_BOOL)

        call property%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_double()
        implicit none
        type(chfl_property) :: property
        integer(chfl_property_kind) :: kind
        real(real64) :: value
        integer :: status

        call property%double(3.4d0, status=status)
        CHECK(status == CHFL_SUCCESS)

        call property%get_double(value, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(value == 3.4d0)

        call property%get_kind(kind, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(kind == CHFL_PROPERTY_DOUBLE)

        call property%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_string()
        implicit none
        type(chfl_property) :: property
        integer(chfl_property_kind) :: kind
        character(len=20) :: value
        integer :: status

        call property%string("test", status=status)
        CHECK(status == CHFL_SUCCESS)

        call property%get_string(value, len(value, 8), status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(value == 'test')

        call property%get_kind(kind, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(kind == CHFL_PROPERTY_STRING)

        call property%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_vector3d()
        implicit none
        type(chfl_property) :: property
        integer(chfl_property_kind) :: kind
        real(real64), dimension(3) :: value
        integer :: status

        call property%vector3d([11d0, 22d0, 33d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        call property%get_vector3d(value, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(value(1) == 11d0)
        CHECK(value(2) == 22d0)
        CHECK(value(3) == 33d0)

        call property%get_kind(kind, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(kind == CHFL_PROPERTY_VECTOR3D)

        call property%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine
end program
