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

contains
    subroutine test_copy()
        implicit none
        type(chfl_frame) :: frame, cloned
        integer(int64) :: natoms
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call cloned%copy(frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 0)
        call cloned%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 0)

        call frame%resize(int(10, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 10)
        call cloned%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 0)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call cloned%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_natoms()
        implicit none
        type(chfl_frame) :: frame
        integer(int64) :: natoms
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 0)

        call frame%resize(int(10, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 10)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_step()
        implicit none
        type(chfl_frame) :: frame
        integer(int64) :: step
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%step(step, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(step == 0)

        call frame%set_step(42_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%step(step, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(step == 42)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_add_atom()
        implicit none
        type(chfl_frame) :: frame
        type(chfl_atom) :: atom
        integer(int64) :: natoms
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%init("Zn", status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%add_atom(atom, [10d0, 20d0, 30d0], status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%add_atom(atom, [10d0, 20d0, 30d0], [10d0, 20d0, 30d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 2)

        call frame%remove(int(0, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 1)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_positions()
        implicit none
        type(chfl_frame) :: frame
        real(real64), dimension(:, :), pointer :: positions, set_positions
        integer(int64) :: natoms, i, j
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%resize(int(4, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%positions(set_positions, natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 4)
        do i=1,3
            do j=1,4
                set_positions(i, j) = real(i * j)
            end do
        end do

        call frame%positions(positions, natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 4)
        do i=1,3
            do j=1,natoms
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
        integer(int64) :: natoms, i, j
        logical(1) :: has_velocities = .true.
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%resize(int(4, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%has_velocities(has_velocities, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(has_velocities .eqv. .false.)

        call frame%add_velocities(status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%has_velocities(has_velocities, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(has_velocities .eqv. .true.)

        call frame%velocities(set_velocities, natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 4)
        do i=1,3
            do j=1,4
                set_velocities(i, j) = real(i * j)
            end do
        end do

        call frame%velocities(velocities, natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        do i=1,3
            do j=1,natoms
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
        real(real64), dimension(3) :: lengths
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call cell%init([3d0, 4d0, 5d0])
        call frame%set_cell(cell, status=status)
        CHECK(status == CHFL_SUCCESS)
        call cell%free()

        call cell%from_frame(frame)
        call cell%lengths(lengths)
        CHECK(all(lengths == [3.0, 4.0, 5.0]))
        call cell%free()

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_topology()
        implicit none
        type(chfl_frame) :: frame
        type(chfl_topology) :: topology
        type(chfl_atom) :: Zn, Ar, atom
        character(len=32) :: name
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%resize(int(4, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%init()
        call Zn%init("Zn")
        call Ar%init("Ar")
        call topology%add_atom(Zn)
        call topology%add_atom(Zn)
        call topology%add_atom(Ar)
        call topology%add_atom(Ar)

        call frame%set_topology(topology, status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%free()
        call Zn%free()
        call Ar%free()

        call topology%from_frame(frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%from_topology(topology, 0_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%free()

        call atom%name(name, len(name, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(name == 'Zn')
        call atom%free()

        call atom%from_frame(frame, 3_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call atom%name(name, len(name, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(name == 'Ar')
        call atom%free()

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine
end program
