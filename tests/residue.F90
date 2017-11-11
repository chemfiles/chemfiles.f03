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

contains
    subroutine test_copy()
        implicit none
        type(chfl_residue) :: residue, cloned
        integer :: status

        call residue%init("Res", status=status)
        CHECK(status == 0)

        call residue%add_atom(int(0, int64), status=status)
        CHECK(status == 0)

        call cloned%copy(residue, status=status)
        CHECK(status == 0)


        ! TODO: test size change


        call residue%free(status=status)
        CHECK(status == 0)
        call cloned%free(status=status)
        CHECK(status == 0)
    end subroutine

    subroutine test_name()
        implicit none
        type(chfl_residue) :: residue
        character(len=32) :: name
        integer :: status

        call residue%init("Foo", status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%name(name, len(name, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(name == 'Foo')

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_id()
        implicit none
        type(chfl_residue) :: residue
        integer(int64) :: id
        integer :: status

        call residue%with_id("Foo", int(56, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%id(id, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(id == 56)

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%init("Bar", status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%id(id, status=status)
        CHECK(status == CHFL_GENERIC_ERROR)

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_size()
        implicit none
        type(chfl_residue) :: residue
        integer(int64) :: natoms
        integer :: status

        call residue%init("Foo", status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 0)

        call residue%add_atom(int(0, int64), status=status)
        CHECK(status == 0)
        call residue%add_atom(int(1, int64), status=status)
        CHECK(status == 0)
        call residue%add_atom(int(2, int64), status=status)
        CHECK(status == 0)

        call residue%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 3)

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_contains()
        implicit none
        type(chfl_residue) :: residue
        logical(1) :: contains
        integer(int64) :: natoms
        integer(int64), dimension(3) :: atoms, expected
        integer :: status, i

        call residue%init("Foo", status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%add_atom(int(0, int64), status=status)
        CHECK(status == 0)
        call residue%add_atom(int(1, int64), status=status)
        CHECK(status == 0)
        call residue%add_atom(int(2, int64), status=status)
        CHECK(status == 0)

        call residue%contains(int(2, int64), contains, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(contains .eqv. .true.)

        call residue%contains(int(20, int64), contains, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(contains .eqv. .false.)

        call residue%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 3)

        call residue%atoms(atoms, natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        expected = [0, 1, 2]
        do i=1,3
            CHECK(atoms(i) == expected(i))
        enddo

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine
end program
