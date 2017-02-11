program frame_test
    use iso_fortran_env, only: real64, int64
    use chemfiles
    use testing
    implicit none

    call test_natoms()
    call test_step()
    call test_positions()
    call test_velocities()
    call test_cell()
    call test_topology()
    call test_velocities()

contains
    subroutine test_natoms()
        implicit none
        type(chfl_frame) :: frame
        integer(int64) :: natoms
        integer :: status

        call frame%init(status=status)
        call check(status == CHFL_SUCCESS, "frame%init")

        call frame%atoms_count(natoms, status=status)
        call check(status == CHFL_SUCCESS, "frame%atoms_count")
        call check(natoms == 0, "frame%atoms_count")

        call frame%resize(int(10, int64), status=status)
        call check(status == CHFL_SUCCESS, "frame%resize")

        call frame%atoms_count(natoms, status=status)
        call check(status == CHFL_SUCCESS, "frame%atoms_count")
        call check(natoms == 10, "frame%atoms_count")

        call frame%free(status=status)
        call check(status == CHFL_SUCCESS, "frame%free")
    end subroutine

    subroutine test_step()
        implicit none
        type(chfl_frame) :: frame
        integer(int64) :: step
        integer :: status

        call frame%init(status=status)
        call check(status == CHFL_SUCCESS, "frame%init")

        call frame%step(step, status=status)
        call check(status == CHFL_SUCCESS, "frame%step")
        call check(step == 0, "frame%step")

        call frame%set_step(42_int64, status=status)
        call check(status == CHFL_SUCCESS, "frame%set_step")

        call frame%step(step, status=status)
        call check(status == CHFL_SUCCESS, "frame%step")
        call check(step == 42, "frame%set_step")

        call frame%free(status=status)
        call check(status == CHFL_SUCCESS, "frame%free")
    end subroutine

    subroutine test_positions()
        implicit none
        type(chfl_frame) :: frame
        real(real64), dimension(:, :), pointer :: positions, set_positions
        integer(int64) :: natoms, i, j
        integer :: status

        call frame%init(status=status)
        call check(status == CHFL_SUCCESS, "frame%init")
        call frame%resize(int(4, int64), status=status)
        call check(status == CHFL_SUCCESS, "frame%resize")

        call frame%positions(set_positions, natoms, status=status)
        call check(status == CHFL_SUCCESS, "frame%positions")
        call check(natoms == 4, "frame%positions")
        do i=1,3
            do j=1,4
                set_positions(i, j) = real(i * j)
            end do
        end do

        call frame%positions(positions, natoms, status=status)
        call check(status == CHFL_SUCCESS, "frame%positions")
        call check(natoms == 4, "frame%positions")
        do i=1,3
            do j=1,natoms
                call check(abs(positions(i, j) - real(i * j)) < 1e-9, "frame%positions")
            end do
        end do

        call frame%free(status=status)
        call check(status == CHFL_SUCCESS, "frame%free")
    end subroutine

    subroutine test_velocities()
        implicit none
        type(chfl_frame) :: frame
        real(real64), dimension(:, :), pointer :: velocities, set_velocities
        integer(int64) :: natoms, i, j
        logical(1) :: has_velocities = .true.
        integer :: status

        call frame%init(status=status)
        call check(status == CHFL_SUCCESS, "frame%init")
        call frame%resize(int(4, int64), status=status)
        call check(status == CHFL_SUCCESS, "frame%resize")

        call frame%has_velocities(has_velocities, status=status)
        call check(status == CHFL_SUCCESS, "frame%has_velocities")
        call check(has_velocities .eqv. .false., "frame%has_velocities")

        call frame%add_velocities(status=status)
        call check(status == CHFL_SUCCESS, "frame%add_velocities")

        call frame%has_velocities(has_velocities, status=status)
        call check(status == CHFL_SUCCESS, "frame%has_velocities")
        call check(has_velocities .eqv. .true., "frame%has_velocities")

        call frame%velocities(set_velocities, natoms, status=status)
        call check(status == CHFL_SUCCESS, "frame%velocities")
        call check(natoms == 4, "frame%velocities")
        do i=1,3
            do j=1,4
                set_velocities(i, j) = real(i * j)
            end do
        end do

        call frame%velocities(velocities, natoms, status=status)
        call check(status == CHFL_SUCCESS, "frame%velocities")
        do i=1,3
            do j=1,natoms
                call check(abs(velocities(i, j) - real(i * j)) < 1e-9, "frame%velocities")
            end do
        end do

        call frame%free(status=status)
        call check(status == CHFL_SUCCESS, "frame%free")
    end subroutine

    subroutine test_cell()
        implicit none
        type(chfl_frame) :: frame
        type(chfl_cell) :: cell
        real(real64), dimension(3) :: lengths
        integer :: status

        call frame%init(status=status)
        call check(status == CHFL_SUCCESS, "frame%init")

        call cell%init([3d0, 4d0, 5d0])
        call frame%set_cell(cell, status=status)
        call check(status == CHFL_SUCCESS, "frame%set_cell")
        call cell%free()

        call cell%from_frame(frame)
        call cell%lengths(lengths)
        call check(all(lengths == [3.0, 4.0, 5.0]), "frame%cell")
        call cell%free()

        call frame%free(status=status)
        call check(status == CHFL_SUCCESS, "frame%free")
    end subroutine

    subroutine test_topology()
        implicit none
        type(chfl_frame) :: frame
        type(chfl_topology) :: topology
        type(chfl_atom) :: Zn, Ar, atom
        character(len=32) :: name
        integer :: status

        call frame%init(status=status)
        call check(status == CHFL_SUCCESS, "frame%init")
        call frame%resize(int(4, int64), status=status)
        call check(status == CHFL_SUCCESS, "frame%resize")

        call topology%init()
        call Zn%init("Zn")
        call Ar%init("Ar")
        call topology%add_atom(Zn)
        call topology%add_atom(Zn)
        call topology%add_atom(Ar)
        call topology%add_atom(Ar)

        call frame%set_topology(topology, status=status)
        call check(status == CHFL_SUCCESS, "frame%set_topology")

        call topology%free()
        call Zn%free()
        call Ar%free()

        call topology%from_frame(frame, status=status)
        call check(status == CHFL_SUCCESS, "topology%from_frame")

        call atom%from_topology(topology, 0_int64, status=status)
        call check(status == CHFL_SUCCESS, "atom%from_topology")
        call topology%free()

        call atom%name(name, len(name, int64), status=status)
        call check(status == CHFL_SUCCESS, "atom%name")
        call check(name == "Zn", "atom%name")
        call atom%free()

        call atom%from_frame(frame, 3_int64, status=status)
        call check(status == CHFL_SUCCESS, "atom%from_frame")
        call atom%name(name, len(name, int64), status=status)
        call check(status == CHFL_SUCCESS, "atom%name")
        call check(name == "Ar", "atom%name")
        call atom%free()

        call frame%free(status=status)
        call check(status == CHFL_SUCCESS, "frame%free")
    end subroutine
end program
