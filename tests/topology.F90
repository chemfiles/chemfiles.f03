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
    call test_impropers()
    call test_residues()

contains
    subroutine test_copy()
        implicit none
        type(chfl_topology) :: topology, cloned
        integer :: status

        call topology%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call cloned%init(topology, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%atoms_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)
        CHECK(cloned%atoms_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)

        call topology%resize(10_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%atoms_count(status=status) == 10)
        CHECK(status == CHFL_SUCCESS)
        CHECK(cloned%atoms_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call cloned%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_size()
        implicit none
        type(chfl_topology) :: topology
        integer :: status

        call topology%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%atoms_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)

        call topology%resize(90_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%atoms_count(status=status) == 90)
        CHECK(status == CHFL_SUCCESS)

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_atoms()
        implicit none
        type(chfl_topology) :: topology
        type(chfl_atom) :: O, H, atom
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

        CHECK(topology%atoms_count(status=status) == 4)
        CHECK(status == CHFL_SUCCESS)

        call O%free()
        call H%free()

        atom = topology%atom(2_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(atom%name(status=status) == 'O')
        CHECK(status == CHFL_SUCCESS)

        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%remove(3_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(topology%atoms_count(status=status) == 3)
        CHECK(status == CHFL_SUCCESS)

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_bonds()
        implicit none
        type(chfl_topology) :: topology
        integer(int64), dimension(2, 3) :: bonds, expected
        integer :: status

        call topology%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%resize(4_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%bonds_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)

        call topology%add_bond(0_int64, 1_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(1_int64, 2_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(2_int64, 3_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%bonds_count(status=status) == 3)
        CHECK(status == CHFL_SUCCESS)

        expected = reshape([0, 1, 1, 2, 2, 3], [2, 3])
        call topology%bonds(bonds, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(all(bonds == expected))

        call topology%remove_bond(2_int64, 3_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(topology%bonds_count(status=status) == 2)
        CHECK(status == CHFL_SUCCESS)

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_angles()
        implicit none
        type(chfl_topology) :: topology
        integer(int64), dimension(3, 2) :: angles, expected
        integer :: status

        call topology%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%resize(4_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%angles_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)

        call topology%add_bond(0_int64, 1_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(1_int64, 2_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(2_int64, 3_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%angles_count(status=status) == 2)
        CHECK(status == CHFL_SUCCESS)

        expected = reshape([0, 1, 2, 1, 2, 3], [3, 2])
        call topology%angles(angles, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(all(angles == expected))

        call topology%remove_bond(2_int64, 3_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%angles_count(status=status) == 1)
        CHECK(status == CHFL_SUCCESS)

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_dihedrals()
        implicit none
        type(chfl_topology) :: topology
        integer(int64), dimension(4, 1) :: dihedrals, expected
        integer :: status

        call topology%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%resize(4_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%dihedrals_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)

        call topology%add_bond(0_int64, 1_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(1_int64, 2_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(2_int64, 3_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%dihedrals_count(status=status) == 1)
        CHECK(status == CHFL_SUCCESS)

        expected = reshape([0, 1, 2, 3], [4, 1])
        call topology%dihedrals(dihedrals, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(all(dihedrals == expected))

        call topology%remove_bond(2_int64, 3_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%dihedrals_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_impropers()
        implicit none
        type(chfl_topology) :: topology
        integer(int64), dimension(4, 1) :: impropers, expected
        integer :: status

        call topology%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%resize(4_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%impropers_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)

        call topology%add_bond(0_int64, 1_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(0_int64, 2_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(0_int64, 3_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%impropers_count(status=status) == 1)
        CHECK(status == CHFL_SUCCESS)

        expected = reshape([1, 0, 2, 3], [4, 1])
        call topology%impropers(impropers, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(all(impropers == expected))

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_residues()
        implicit none
        type(chfl_topology) :: topology
        type(chfl_atom) :: atom
        type(chfl_residue) :: residue, first, second
        integer :: status

        call atom%init("X", status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%residues_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)

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

        call residue%add_atom(0_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call residue%add_atom(1_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%add_residue(residue, status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%init("bar", status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%add_atom(2_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%add_residue(residue, status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%residues_count(status=status) == 2)
        CHECK(status == CHFL_SUCCESS)

        first = topology%residue(0_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(first%name(status=status) == 'foo')
        CHECK(status == CHFL_SUCCESS)

        second = topology%residue_for_atom(3_int64, status=status)
        CHECK(status /= CHFL_SUCCESS)

        second = topology%residue_for_atom(2_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(second%name(status=status) == 'bar')
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%residues_linked(first, second, status=status) .eqv. .false.)
        CHECK(status == CHFL_SUCCESS)

        call topology%add_bond(1_int64, 2_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%residues_linked(first, second, status=status) .eqv. .true.)
        CHECK(status == CHFL_SUCCESS)

        call first%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call second%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine
end program
