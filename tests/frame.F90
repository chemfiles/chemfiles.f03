#include "check.inc"

program frame_test
    use iso_fortran_env, only: real64, int64
    use chemfiles
    implicit none

    call test_copy()
    call test_add_atom()
    call test_natoms()
    call test_step()
    call test_positions()
    call test_velocities()
    call test_cell()
    call test_topology()
    call test_velocities()
    ! call test_properties()
    call test_distances()
    call test_bonds()
    call test_residues()

contains
    subroutine test_copy()
        implicit none
        type(chfl_frame) :: frame, cloned
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call cloned%init(frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(frame%atoms_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)
        CHECK(cloned%atoms_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)

        call frame%resize(10_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(frame%atoms_count(status=status) == 10)
        CHECK(status == CHFL_SUCCESS)
        CHECK(cloned%atoms_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call cloned%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_natoms()
        implicit none
        type(chfl_frame) :: frame
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(frame%atoms_count(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)

        call frame%resize(int(10, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(frame%atoms_count(status=status) == 10)
        CHECK(status == CHFL_SUCCESS)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_step()
        implicit none
        type(chfl_frame) :: frame
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(frame%step(status=status) == 0)
        CHECK(status == CHFL_SUCCESS)

        call frame%set_step(42_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(frame%step(status=status) == 42)
        CHECK(status == CHFL_SUCCESS)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_add_atom()
        implicit none
        type(chfl_frame) :: frame
        type(chfl_atom) :: atom
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%init("Zn", status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%add_atom(atom, [10d0, 20d0, 30d0], status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%add_atom(atom, [10d0, 20d0, 30d0], [10d0, 20d0, 30d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(frame%atoms_count(status=status) == 2)
        CHECK(status == CHFL_SUCCESS)

        call frame%remove(0_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(frame%atoms_count(status=status) == 1)
        CHECK(status == CHFL_SUCCESS)

        atom = frame%atom(0_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(atom%name(status=status) == 'Zn')
        CHECK(status == CHFL_SUCCESS)
        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_positions()
        implicit none
        type(chfl_frame) :: frame
        real(real64), dimension(:, :), pointer :: positions, set_positions
        integer :: i, j
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%resize(4_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        set_positions => frame%positions(status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(all(shape(set_positions) == [3, 4]))
        do i=1,3
            do j=1,4
                set_positions(i, j) = real(i * j)
            end do
        end do

        positions => frame%positions(status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(all(shape(positions) == [3, 4]))
        do i=1,3
            do j=1,4
                CHECK(abs(positions(i, j) - real(i * j)) < 1e-9)
            end do
        end do

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_velocities()
        implicit none
        type(chfl_frame) :: frame
        real(real64), dimension(:, :), pointer :: velocities, set_velocities
        integer :: i, j
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%resize(4_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(frame%has_velocities(status=status) .eqv. .false.)
        CHECK(status == CHFL_SUCCESS)

        call frame%add_velocities(status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(frame%has_velocities(status=status) .eqv. .true.)
        CHECK(status == CHFL_SUCCESS)

        set_velocities => frame%velocities(status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(all(shape(set_velocities) == [3, 4]))
        do i=1,3
            do j=1,4
                set_velocities(i, j) = real(i * j)
            end do
        end do

        velocities => frame%velocities(status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(all(shape(velocities) == [3, 4]))
        do i=1,3
            do j=1,4
                CHECK(abs(velocities(i, j) - real(i * j)) < 1e-9)
            end do
        end do

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_cell()
        implicit none
        type(chfl_frame) :: frame
        type(chfl_cell) :: cell
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call cell%init([3d0, 4d0, 5d0])
        call frame%set_cell(cell, status=status)
        CHECK(status == CHFL_SUCCESS)
        call cell%free()

        cell = frame%cell(status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(all(cell%lengths(status=status) == [3.0, 4.0, 5.0]))
        CHECK(status == CHFL_SUCCESS)
        call cell%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_topology()
        implicit none
        type(chfl_frame) :: frame
        type(chfl_topology) :: topology
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%resize(4_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%resize(4_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(1_int64, 2_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(1_int64, 3_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%set_topology(topology, status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        topology = frame%topology(status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(topology%bonds_count(status=status) == 2)
        CHECK(status == CHFL_SUCCESS)
        CHECK(topology%atoms_count(status=status) == 4)
        CHECK(status == CHFL_SUCCESS)
        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    ! subroutine test_properties()
    !     implicit none
    !     type(chfl_frame) :: frame
    !     type(chfl_property) :: property
    !     real(real64) :: value
    !     integer :: status
    !
    !     call frame%init(status=status)
    !     CHECK(status == CHFL_SUCCESS)
    !
    !     call property%double(42d0, status=status)
    !     CHECK(status == CHFL_SUCCESS)
    !
    !     call frame%set_property("foo", property, status=status)
    !     CHECK(status == CHFL_SUCCESS)
    !
    !     call property%free(status=status)
    !     CHECK(status == CHFL_SUCCESS)
    !
    !     call property%from_frame(frame, "foo", status=status)
    !     CHECK(status == CHFL_SUCCESS)
    !
    !     call property%get_double(value, status=status)
    !     CHECK(status == CHFL_SUCCESS)
    !     CHECK(value == 42d0)
    !
    !     call property%free(status=status)
    !     CHECK(status == CHFL_SUCCESS)
    !     call frame%free(status=status)
    !     CHECK(status == CHFL_SUCCESS)
    ! end subroutine

    subroutine test_distances()
        implicit none
        type(chfl_frame) :: frame
        type(chfl_atom) :: atom
        real(real64), parameter :: pi = 3.14159265358979323846264338327950288d0
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%init("Zn", status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%add_atom(atom, [1d0, 0d0, 0d0], status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%add_atom(atom, [0d0, 0d0, 0d0], status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%add_atom(atom, [0d0, 1d0, 0d0], status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%add_atom(atom, [0d0, 1d0, 1d0], status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%add_atom(atom, [0d0, 1d0, 2d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(frame%distance(0_int64, 2_int64, status=status) == sqrt(2d0))
        CHECK(status == CHFL_SUCCESS)

        CHECK(frame%angle(0_int64, 1_int64, 2_int64, status=status) == pi / 2d0)
        CHECK(status == CHFL_SUCCESS)

        CHECK(frame%dihedral(0_int64, 1_int64, 2_int64, 3_int64, status=status) == pi / 2d0);
        CHECK(status == CHFL_SUCCESS)

        CHECK(frame%out_of_plane(1_int64, 4_int64, 0_int64, 2_int64, status=status) == 2d0)
        CHECK(status == CHFL_SUCCESS)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_bonds()
        implicit none
        type(chfl_frame) :: frame
        type(chfl_atom) :: atom
        type(chfl_topology) :: topology
        integer(int64), dimension(2, 3) :: bonds, expected
        integer :: status, i, j

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%init("", status=status)
        CHECK(status == CHFL_SUCCESS)
        do i = 1,5
            call frame%add_atom(atom, [1d0, 0d0, 0d0], status=status)
            CHECK(status == CHFL_SUCCESS)
        enddo
        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%add_bond(0_int64, 2_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%add_bond(1_int64, 2_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%add_bond(3_int64, 0_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        topology = frame%topology(status=status)
        CHECK(status == CHFL_SUCCESS)

        expected = reshape([0, 2, 0, 3, 1, 2], [2, 3])
        call topology%bonds(bonds, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(all(shape(bonds) == [2, 3]))
        do i=1,2
            do j=1,3
                CHECK(bonds(i, j) == expected(i, j))
            end do
        end do

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%remove_bond(3_int64, 0_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        ! Non existant bonds, to check that this is fine to do
        call frame%remove_bond(3_int64, 0_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%remove_bond(2_int64, 3_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        topology = frame%topology(status=status)
        CHECK(status == CHFL_SUCCESS)

        expected = reshape([0, 2, 1, 2, -1, -1], [2, 3])
        call topology%bonds(bonds, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(all(shape(bonds) == [2, 2]))
        do i=1,2
            do j=1,2
                CHECK(bonds(i, j) == expected(i, j))
            end do
        end do
        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_residues()
        implicit none
        type(chfl_frame) :: frame
        type(chfl_residue) :: residue
        type(chfl_topology) :: topology
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%init("foobar", status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%add_residue(residue, status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%add_residue(residue, status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%add_residue(residue, status=status)
        CHECK(status == CHFL_SUCCESS)

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        topology = frame%topology(status=status)
        CHECK(status == CHFL_SUCCESS)

        residue = topology%residue(0_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(residue%name(status=status) == 'foobar')
        CHECK(status == CHFL_SUCCESS)

        call residue%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine
end program
