program topology_test
    use iso_fortran_env, only: real64, int64
    use chemfiles
    use testing
    implicit none

    call test_size()
    call test_atoms()
    call test_bonds()
    call test_angles()
    call test_dihedrals()

contains
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
        call check(status == CHFL_SUCCESS, "topology%natoms")

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
        integer(int64) :: n, i, j
        integer(int64), dimension(2, 3) :: bonds, expected
        logical(1) :: isbond
        integer :: status

        call topology%init(status=status)
        call check(status == CHFL_SUCCESS, "topology%init")

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

        call topology%isbond(int(0, int64), int(1, int64), isbond, status=status)
        call check(status == CHFL_SUCCESS, "topology%isbond")
        call check(isbond .eqv. .true., "topology%isbond")
        call topology%isbond(int(0, int64), int(3, int64), isbond, status=status)
        call check(status == CHFL_SUCCESS, "topology%isbond")
        call check(isbond .eqv. .false., "topology%isbond")

        call topology%isbond(int(0, int64), int(1, int64), isbond, status=status)
        call check(status == CHFL_SUCCESS, "topology%isbond")
        call check(isbond .eqv. .true., "topology%isbond")
        call topology%isbond(int(0, int64), int(3, int64), isbond, status=status)
        call check(status == CHFL_SUCCESS, "topology%isbond")
        call check(isbond .eqv. .false., "topology%isbond")


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
        integer(int64) :: n, i, j
        integer(int64), dimension(3, 2) :: angles, expected
        logical(1) :: isangle
        integer :: status

        call topology%init(status=status)
        call check(status == CHFL_SUCCESS, "topology%init")

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

        call topology%isangle(int(0, int64), int(1, int64), int(2, int64), isangle, status=status)
        call check(status == CHFL_SUCCESS, "topology%isangle")
        call check(isangle .eqv. .true., "topology%isangle")
        call topology%isangle(int(0, int64), int(1, int64), int(3, int64), isangle, status=status)
        call check(status == CHFL_SUCCESS, "topology%isangle")
        call check(isangle .eqv. .false., "topology%isangle")

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
        integer(int64) :: n, i
        integer(int64), dimension(4, 1) :: dihedrals, expected
        logical(1) :: isdihedral
        integer :: status

        call topology%init(status=status)
        call check(status == CHFL_SUCCESS, "topology%init")

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

        call topology%isdihedral(int(0, int64), int(1, int64), int(2, int64), int(3, int64), isdihedral, status=status)
        call check(status == CHFL_SUCCESS, "topology%isdihedral")
        call check(isdihedral .eqv. .true., "topology%isdihedral")
        call topology%isdihedral(int(0, int64), int(1, int64), int(3, int64), int(2, int64), isdihedral, status=status)
        call check(status == CHFL_SUCCESS, "topology%isdihedral")
        call check(isdihedral .eqv. .false., "topology%isdihedral")

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
end program
