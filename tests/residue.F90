#include "check.inc"

program residue_test
    use iso_fortran_env, only: int64
    use chemfiles
    implicit none

    call test_copy()
    call test_name()
    call test_id()
    call test_size()
    call test_contains()
    call test_properties()

contains
    subroutine test_copy()
        implicit none
        type(chfl_residue) :: residue, cloned
        integer :: status

        call residue%init("Res", status=status)
        CHECK(status == CHFL_SUCCESS)
        call cloned%init(residue, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(residue%atoms_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)
        CHECK(cloned%atoms_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)

        call residue%add_atom(0_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(residue%atoms_count(status=status) == 1)
        CHECK(status == CHFL_SUCCESS)
        CHECK(cloned%atoms_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        ! Call free twice to check that it works
        call cloned%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call cloned%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_name()
        implicit none
        type(chfl_residue) :: residue
        integer :: status

        call residue%init("Foo", status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(residue%name(status=status) == 'Foo')
        CHECK(status == CHFL_SUCCESS)

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_id()
        implicit none
        type(chfl_residue) :: residue
        integer :: status

        call residue%init("Foo", 56_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(residue%id(status=status) == 56)
        CHECK(status == CHFL_SUCCESS)

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_size()
        implicit none
        type(chfl_residue) :: residue
        integer :: status

        call residue%init("Foo", status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(residue%atoms_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)

        call residue%add_atom(0_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call residue%add_atom(1_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call residue%add_atom(2_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(residue%atoms_count(status=status) == 3)
        CHECK(status == CHFL_SUCCESS)

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_contains()
        implicit none
        type(chfl_residue) :: residue
        integer(int64), dimension(3) :: atoms
        integer :: status

        call residue%init("Foo", status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%add_atom(0_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call residue%add_atom(1_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call residue%add_atom(2_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(residue%contains(2_int64, status=status) .eqv. .true.)
        CHECK(status == CHFL_SUCCESS)

        CHECK(residue%contains(20_int64, status=status) .eqv. .false.)
        CHECK(status == CHFL_SUCCESS)

        CHECK(residue%atoms_count(status=status) == 3)
        CHECK(status == CHFL_SUCCESS)

        call residue%atoms(atoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(all(atoms == [0, 1, 2]))

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_properties()
        implicit none
        type(chfl_residue) :: residue
        type(chfl_property) :: property
        character(len=CHFL_STRING_LENGTH), dimension(2) :: names
        integer :: status

        call residue%init('He', status=status)
        CHECK(status == CHFL_SUCCESS)

        call property%init(42d0, status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%set("foo", property, status=status)
        CHECK(status == CHFL_SUCCESS)
        call residue%set("bar", .false., status=status)
        CHECK(status == CHFL_SUCCESS)

        call property%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        property = residue%get("foo", status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(property%double(status=status) == 42d0)
        CHECK(status == CHFL_SUCCESS)
        call property%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        property = residue%get("bar", status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(property%bool(status=status) .eqv. .false.)
        CHECK(status == CHFL_SUCCESS)
        call property%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        property = residue%get("baz", status=status)
        CHECK(status /= CHFL_SUCCESS)

        CHECK(residue%properties_count(status=status) == 2)
        CHECK(status == CHFL_SUCCESS)

        call residue%list_properties(names, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(names(1) == 'bar')
        CHECK(names(2) == 'foo')

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine
end program
