#include "check.inc"

program topology_test
    use iso_fortran_env, only: real64, int64
    use chemfiles
    implicit none

    call test_copy()
    call test_size()
    call test_atoms()
    call test_bonds()
    call test_angles()
    call test_dihedrals()
    call test_residues()

contains
    subroutine test_copy()
        implicit none
        type(chfl_topology) :: topology, cloned
        integer(int64) :: natoms
        integer :: status

        call topology%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%resize(int(90, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call cloned%copy(topology, status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 90)
        natoms = 0
        call cloned%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 90)

        call topology%resize(int(10, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 10)
        natoms = 0
        call cloned%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 90)

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call cloned%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_size()
        implicit none
        type(chfl_topology) :: topology
        integer(int64) :: natoms = 100
        integer :: status

        call topology%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 0)

        call topology%resize(int(90, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 90)

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_atoms()
        implicit none
        type(chfl_topology) :: topology
        type(chfl_atom) :: O, H, atom
        integer(int64) :: natoms = 0
        character(len=32) :: name
        integer :: status

        call topology%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        ! Creating some H2O2
        call O%init("O")
        call H%init("H")

        call topology%add_atom(H, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_atom(O, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_atom(O, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_atom(H, status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 4)

        call O%free()
        call H%free()

        call atom%from_topology(topology, int(2, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%name(name, len(name, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(name == 'O')

        call atom%free()

        call topology%remove(int(3, int64), status=status)
        call topology%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 3)

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_bonds()
        implicit none
        type(chfl_topology) :: topology
        type(chfl_atom) :: atom
        integer(int64) :: n, i, j
        integer(int64), dimension(2, 3) :: bonds, expected
        integer :: status

        call topology%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%init("", status=status)
        CHECK(status == CHFL_SUCCESS)
        do i = 1,4
            call topology%add_atom(atom, status=status)
            CHECK(status == CHFL_SUCCESS)
        enddo
        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%bonds_count(n, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(n == 0)

        call topology%add_bond(int(0, int64), int(1, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(int(1, int64), int(2, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(int(2, int64), int(3, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%bonds_count(n, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(n == 3)

        expected = reshape([0, 1, 1, 2, 2, 3], [2, 3])
        call topology%bonds(bonds, int(3, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        do i=1,2
            do j=1,3
                CHECK(bonds(i, j) == expected(i, j))
            end do
        end do

        call topology%remove_bond(int(2, int64), int(3, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%bonds_count(n, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(n == 2)

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_angles()
        implicit none
        type(chfl_topology) :: topology
        type(chfl_atom) :: atom
        integer(int64) :: n, i, j
        integer(int64), dimension(3, 2) :: angles, expected
        integer :: status

        call topology%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%init("", status=status)
        CHECK(status == CHFL_SUCCESS)
        do i = 1,4
            call topology%add_atom(atom, status=status)
            CHECK(status == CHFL_SUCCESS)
        enddo
        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%angles_count(n, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(n == 0)

        call topology%add_bond(int(0, int64), int(1, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(int(1, int64), int(2, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(int(2, int64), int(3, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%angles_count(n, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(n == 2)

        expected = reshape([0, 1, 2, 1, 2, 3], [3, 2])
        call topology%angles(angles, int(2, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        do i=1,3
            do j=1,2
                CHECK(angles(i, j) == expected(i, j))
            end do
        end do

        call topology%remove_bond(int(2, int64), int(3, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%angles_count(n, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(n == 1)

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_dihedrals()
        implicit none
        type(chfl_topology) :: topology
        type(chfl_atom) :: atom
        integer(int64) :: n, i
        integer(int64), dimension(4, 1) :: dihedrals, expected
        integer :: status

        call topology%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%init("", status=status)
        CHECK(status == CHFL_SUCCESS)
        do i = 1,4
            call topology%add_atom(atom, status=status)
            CHECK(status == CHFL_SUCCESS)
        enddo
        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%dihedrals_count(n, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(n == 0)

        call topology%add_bond(int(0, int64), int(1, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(int(1, int64), int(2, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(int(2, int64), int(3, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%dihedrals_count(n, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(n == 1)

        expected = reshape([0, 1, 2, 3], [4, 1])
        call topology%dihedrals(dihedrals, int(1, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        do i=1,4
            CHECK(dihedrals(i, 1) == expected(i, 1))
        end do

        call topology%remove_bond(int(2, int64), int(3, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%dihedrals_count(n, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(n == 0)

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_residues()
        implicit none
        type(chfl_topology) :: topology
        type(chfl_atom) :: atom
        type(chfl_residue) :: residue, first, second
        logical(1) :: linked
        character(len=32) :: name
        integer(int64) :: n
        integer :: status

        call atom%init("X", status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%residues_count(n, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(n == 0)

        call topology%add_atom(atom, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_atom(atom, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_atom(atom, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_atom(atom, status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%init("foo", status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%add_atom(int(0, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        call residue%add_atom(int(1, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%add_residue(residue, status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%init("bar", status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%add_atom(int(2, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%add_residue(residue, status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%residues_count(n, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(n == 2)

        call first%from_topology(topology, int(0, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call first%name(name, len(name, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(name == 'foo')

        call second%for_atom(topology, int(3, int64), status=status)
        CHECK(status /= CHFL_SUCCESS)

        call second%for_atom(topology, int(2, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        call second%name(name, len(name, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(name == 'bar')

        call topology%residues_linked(first, second, linked, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(linked .eqv. .false.)

        call topology%add_bond(int(1, int64), int(2, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%residues_linked(first, second, linked, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(linked .eqv. .true.)


        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call first%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call second%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine
end program
