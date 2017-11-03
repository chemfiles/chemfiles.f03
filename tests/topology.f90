program topology_test
    use iso_fortran_env, only: real64, int64
    use chemfiles
    use testing
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
        call check(status == CHFL_SUCCESS, "topology%init")
        call topology%resize(int(90, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%resize")

        call cloned%copy(topology, status=status)
        call check(status == CHFL_SUCCESS, "topology%copy")

        call topology%atoms_count(natoms, status=status)
        call check(status == CHFL_SUCCESS, "topology%natoms")
        call check(natoms == 90, "topology%natoms")
        natoms = 0
        call cloned%atoms_count(natoms, status=status)
        call check(status == CHFL_SUCCESS, "topology%natoms")
        call check(natoms == 90, "topology%natoms")

        call topology%resize(int(10, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%resize")

        call topology%atoms_count(natoms, status=status)
        call check(status == CHFL_SUCCESS, "topology%natoms")
        call check(natoms == 10, "topology%natoms")
        natoms = 0
        call cloned%atoms_count(natoms, status=status)
        call check(status == CHFL_SUCCESS, "topology%natoms")
        call check(natoms == 90, "topology%natoms")

        call topology%free(status=status)
        call check(status == CHFL_SUCCESS, "topology%free")
        call cloned%free(status=status)
        call check(status == CHFL_SUCCESS, "topology%free")
    end subroutine

    subroutine test_size()
        implicit none
        type(chfl_topology) :: topology
        integer(int64) :: natoms = 100
        integer :: status

        call topology%init(status=status)
        call check(status == CHFL_SUCCESS, "topology%init")

        call topology%atoms_count(natoms, status=status)
        call check(status == CHFL_SUCCESS, "topology%natoms")
        call check(natoms == 0, "topology%natoms")

        call topology%resize(int(90, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%resize")

        call topology%atoms_count(natoms, status=status)
        call check(status == CHFL_SUCCESS, "topology%natoms")
        call check(natoms == 90, "topology%natoms")

        call topology%free(status=status)
        call check(status == CHFL_SUCCESS, "topology%free")
    end subroutine

    subroutine test_atoms()
        implicit none
        type(chfl_topology) :: topology
        type(chfl_atom) :: O, H, atom
        integer(int64) :: natoms = 0
        character(len=32) :: name
        integer :: status

        call topology%init(status=status)
        call check(status == CHFL_SUCCESS, "topology%init")

        ! Creating some H2O2
        call O%init("O")
        call H%init("H")

        call topology%add_atom(H, status=status)
        call check(status == CHFL_SUCCESS, "topology%add_atom")
        call topology%add_atom(O, status=status)
        call check(status == CHFL_SUCCESS, "topology%add_atom")
        call topology%add_atom(O, status=status)
        call check(status == CHFL_SUCCESS, "topology%add_atom")
        call topology%add_atom(H, status=status)
        call check(status == CHFL_SUCCESS, "topology%add_atom")

        call topology%atoms_count(natoms, status=status)
        call check(status == CHFL_SUCCESS, "topology%atoms_count")
        call check(natoms == 4, "topology%atoms_count")

        call O%free()
        call H%free()

        call atom%from_topology(topology, int(2, int64), status=status)
        call check(status == CHFL_SUCCESS, "atom%from_topology")

        call atom%name(name, len(name, int64), status=status)
        call check(status == CHFL_SUCCESS, "atom%name")
        call check(name == "O", "atom%name")

        call atom%free()

        call topology%remove(int(3, int64), status=status)
        call topology%atoms_count(natoms, status=status)
        call check(status == CHFL_SUCCESS, "topology%atoms_count")
        call check(natoms == 3, "topology%atoms_count")

        call topology%free(status=status)
        call check(status == CHFL_SUCCESS, "topology%free")
    end subroutine

    subroutine test_bonds()
        implicit none
        type(chfl_topology) :: topology
        type(chfl_atom) :: atom
        integer(int64) :: n, i, j
        integer(int64), dimension(2, 3) :: bonds, expected
        integer :: status

        call topology%init(status=status)
        call check(status == CHFL_SUCCESS, "topology%init")

        call atom%init("", status=status)
        call check(status == CHFL_SUCCESS, "atom%init")
        do i = 1,4
            call topology%add_atom(atom, status=status)
            call check(status == CHFL_SUCCESS, "topology%add_atom")
        enddo
        call atom%free(status=status)
        call check(status == CHFL_SUCCESS, "atom%free")

        call topology%bonds_count(n, status=status)
        call check(status == CHFL_SUCCESS, "topology%bonds_count")
        call check(n == 0, "topology%bonds_count")

        call topology%add_bond(int(0, int64), int(1, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%add_bond")
        call topology%add_bond(int(1, int64), int(2, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%add_bond")
        call topology%add_bond(int(2, int64), int(3, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%add_bond")

        call topology%bonds_count(n, status=status)
        call check(status == CHFL_SUCCESS, "topology%bonds_count")
        call check(n == 3, "topology%bonds_count")

        expected = reshape([0, 1, 1, 2, 2, 3], [2, 3])
        call topology%bonds(bonds, int(3, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%bonds")
        do i=1,2
            do j=1,3
                call check(bonds(i, j) == expected(i, j), "topology%bonds")
            end do
        end do

        call topology%remove_bond(int(2, int64), int(3, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%remove_bond")
        call topology%bonds_count(n, status=status)
        call check(status == CHFL_SUCCESS, "topology%bonds_count")
        call check(n == 2, "topology%bonds_count")

        call topology%free(status=status)
        call check(status == CHFL_SUCCESS, "topology%free")
    end subroutine

    subroutine test_angles()
        implicit none
        type(chfl_topology) :: topology
        type(chfl_atom) :: atom
        integer(int64) :: n, i, j
        integer(int64), dimension(3, 2) :: angles, expected
        integer :: status

        call topology%init(status=status)
        call check(status == CHFL_SUCCESS, "topology%init")

        call atom%init("", status=status)
        call check(status == CHFL_SUCCESS, "atom%init")
        do i = 1,4
            call topology%add_atom(atom, status=status)
            call check(status == CHFL_SUCCESS, "topology%add_atom")
        enddo
        call atom%free(status=status)
        call check(status == CHFL_SUCCESS, "atom%free")

        call topology%angles_count(n, status=status)
        call check(status == CHFL_SUCCESS, "topology%angles_count")
        call check(n == 0, "topology%angles_count")

        call topology%add_bond(int(0, int64), int(1, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%add_bond")
        call topology%add_bond(int(1, int64), int(2, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%add_bond")
        call topology%add_bond(int(2, int64), int(3, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%add_bond")

        call topology%angles_count(n, status=status)
        call check(status == CHFL_SUCCESS, "topology%angles_count")
        call check(n == 2, "topology%angles_count")

        expected = reshape([0, 1, 2, 1, 2, 3], [3, 2])
        call topology%angles(angles, int(2, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%angles")
        do i=1,3
            do j=1,2
                call check(angles(i, j) == expected(i, j), "topology%angles")
            end do
        end do

        call topology%remove_bond(int(2, int64), int(3, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%remove_bond")

        call topology%angles_count(n, status=status)
        call check(status == CHFL_SUCCESS, "topology%angles_count")
        call check(n == 1, "topology%angles_count")

        call topology%free(status=status)
        call check(status == CHFL_SUCCESS, "topology%free")
    end subroutine

    subroutine test_dihedrals()
        implicit none
        type(chfl_topology) :: topology
        type(chfl_atom) :: atom
        integer(int64) :: n, i
        integer(int64), dimension(4, 1) :: dihedrals, expected
        integer :: status

        call topology%init(status=status)
        call check(status == CHFL_SUCCESS, "topology%init")

        call atom%init("", status=status)
        call check(status == CHFL_SUCCESS, "atom%init")
        do i = 1,4
            call topology%add_atom(atom, status=status)
            call check(status == CHFL_SUCCESS, "topology%add_atom")
        enddo
        call atom%free(status=status)
        call check(status == CHFL_SUCCESS, "atom%free")

        call topology%dihedrals_count(n, status=status)
        call check(status == CHFL_SUCCESS, "topology%dihedrals_count")
        call check(n == 0, "topology%dihedrals_count")

        call topology%add_bond(int(0, int64), int(1, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%add_bond")
        call topology%add_bond(int(1, int64), int(2, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%add_bond")
        call topology%add_bond(int(2, int64), int(3, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%add_bond")

        call topology%dihedrals_count(n, status=status)
        call check(status == CHFL_SUCCESS, "topology%dihedrals_count")
        call check(n == 1, "topology%dihedrals_count")

        expected = reshape([0, 1, 2, 3], [4, 1])
        call topology%dihedrals(dihedrals, int(1, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%dihedrals")
        do i=1,4
            call check(dihedrals(i, 1) == expected(i, 1), "topology%dihedrals")
        end do

        call topology%remove_bond(int(2, int64), int(3, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%remove_bond")

        call topology%dihedrals_count(n, status=status)
        call check(status == CHFL_SUCCESS, "topology%dihedrals_count")
        call check(n == 0, "topology%dihedrals_count")

        call topology%free(status=status)
        call check(status == CHFL_SUCCESS, "topology%free")
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
        call check(status == CHFL_SUCCESS, "atom%init")

        call topology%init(status=status)
        call check(status == CHFL_SUCCESS, "topology%init")

        call topology%residues_count(n, status=status)
        call check(status == CHFL_SUCCESS, "topology%residues_count")
        call check(n == 0, "topology%residues_count")

        call topology%add_atom(atom, status=status)
        call check(status == CHFL_SUCCESS, "topology%add_atom")
        call topology%add_atom(atom, status=status)
        call check(status == CHFL_SUCCESS, "topology%add_atom")
        call topology%add_atom(atom, status=status)
        call check(status == CHFL_SUCCESS, "topology%add_atom")
        call topology%add_atom(atom, status=status)
        call check(status == CHFL_SUCCESS, "topology%add_atom")

        call atom%free(status=status)
        call check(status == CHFL_SUCCESS, "atom%free")

        call residue%init("foo", status=status)
        call check(status == CHFL_SUCCESS, "residue%init")

        call residue%add_atom(int(0, int64), status=status)
        call check(status == CHFL_SUCCESS, "residue%add_atom")
        call residue%add_atom(int(1, int64), status=status)
        call check(status == CHFL_SUCCESS, "residue%add_atom")

        call topology%add_residue(residue, status=status)
        call check(status == CHFL_SUCCESS, "topology%add_residue")

        call residue%free(status=status)
        call check(status == CHFL_SUCCESS, "residue%free")

        call residue%init("bar", status=status)
        call check(status == CHFL_SUCCESS, "residue%init")

        call residue%add_atom(int(2, int64), status=status)
        call check(status == CHFL_SUCCESS, "residue%add_atom")

        call topology%add_residue(residue, status=status)
        call check(status == CHFL_SUCCESS, "topology%add_residue")

        call residue%free(status=status)
        call check(status == CHFL_SUCCESS, "residue%free")

        call topology%residues_count(n, status=status)
        call check(status == CHFL_SUCCESS, "topology%residues_count")
        call check(n == 2, "topology%residues_count")

        call first%from_topology(topology, int(0, int64), status=status)
        call check(status == CHFL_SUCCESS, "residue%from_topology")

        call first%name(name, len(name, int64), status=status)
        call check(status == CHFL_SUCCESS, "residue%name")
        call check(name == "foo", "residue%name")

        call second%for_atom(topology, int(3, int64), status=status)
        call check(status /= CHFL_SUCCESS, "residue%from_topology")

        call second%for_atom(topology, int(2, int64), status=status)
        call check(status == CHFL_SUCCESS, "residue%from_topology")
        call second%name(name, len(name, int64), status=status)
        call check(status == CHFL_SUCCESS, "residue%name")
        call check(name == "bar", "residue%name")

        call topology%residues_linked(first, second, linked, status=status)
        call check(status == CHFL_SUCCESS, "topology%residues_linked")
        call check(linked .eqv. .false., "topology%residues_linked")

        call topology%add_bond(int(1, int64), int(2, int64), status=status)
        call check(status == CHFL_SUCCESS, "topology%add_bond")

        call topology%residues_linked(first, second, linked, status=status)
        call check(status == CHFL_SUCCESS, "topology%residues_linked")
        call check(linked .eqv. .true., "topology%residues_linked")


        call topology%free(status=status)
        call check(status == CHFL_SUCCESS, "topology%free")
        call first%free(status=status)
        call check(status == CHFL_SUCCESS, "residue%free")
        call second%free(status=status)
        call check(status == CHFL_SUCCESS, "residue%free")
    end subroutine
end program
