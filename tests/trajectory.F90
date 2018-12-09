#include "check.inc"

program trajectory_read
    use iso_fortran_env, only: real64, int64
    use chemfiles
    implicit none

    call test_read()
    call test_open_with_format()
    call test_set_cell()
    call test_set_topology()
    call test_set_topology_from_file()
    call test_guess_bonds()
    call test_write()

contains
    subroutine test_read()
        implicit none
        type(chfl_trajectory) :: file
        type(chfl_frame) :: frame
        real(real64) :: pos_1(3), pos_125(3)
        real(real64), pointer :: positions(:, :)
        integer :: i
        integer :: status

        call file%open("data/water.xyz", "r", status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(file%path(status=status) == 'data/water.xyz')
        CHECK(status == CHFL_SUCCESS)

        CHECK(file%nsteps(status=status) == 100)
        CHECK(status == CHFL_SUCCESS)

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        ! Read the first frame
        call file%read(frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        ! Check positions in the first frame
        pos_1 = [0.417219, 8.303366, 11.737172]
        pos_125 = [5.099554, -0.045104, 14.153846]
        positions => frame%positions(status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(all(shape(positions) == [3, 297]))

        do i=1,3
            CHECK(abs(pos_1(i) - positions(i, 1)) < 1d-6)
            CHECK(abs(pos_125(i) - positions(i, 125)) < 1d-6)
        end do

        ! Check reading a specific step
        call file%read_step(int(41, int64), frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        pos_1(:) = [0.761277d0, 8.106125d0, 10.622949d0]
        pos_125(:) = [5.13242d0, 0.079862d0, 14.194161d0]

        positions => frame%positions(status=status)
        CHECK(status == CHFL_SUCCESS)
        do i=1,3
            CHECK(abs(pos_1(i) - positions(i, 1)) < 1d-6)
            CHECK(abs(pos_125(i) - positions(i, 125)) < 1d-6)
        end do

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        ! Call close twice to check that it works
        call file%close(status=status)
        CHECK(status == CHFL_SUCCESS)
        call file%close(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_open_with_format()
        implicit none
        type(chfl_trajectory) :: file
        type(chfl_frame) :: frame
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%open("data/helium.xyz.but.not.really", "r", "XYZ", status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%read(frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(frame%atoms_count(status=status) == 125)
        CHECK(status == CHFL_SUCCESS)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call file%close(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_set_cell()
        implicit none
        type(chfl_trajectory) :: file
        type(chfl_frame) :: frame
        type(chfl_cell) :: cell
        integer :: status

        ! Set the cell associated with a trajectory
        call cell%init([30d0, 30d0, 30d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%open("data/water.xyz", "r", status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%set_cell(cell, status=status)
        CHECK(status == CHFL_SUCCESS)

        call cell%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%read(frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        ! Check that the cell was set
        cell = frame%cell(status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(all(cell%lengths(status=status) == [30.0, 30.0, 30.0]))
        CHECK(status == CHFL_SUCCESS)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call cell%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call file%close(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_set_topology()
        implicit none
        type(chfl_trajectory) :: file
        type(chfl_frame) :: frame
        type(chfl_topology) :: topology
        type(chfl_atom) :: atom
        integer :: status, i

        call file%open("data/water.xyz", "r", status=status)
        CHECK(status == CHFL_SUCCESS)

        ! Set the topology associated with a trajectory by hand
        call topology%init()
        call atom%init("Cs")

        do i=1,297
            call topology%add_atom(atom, status=status)
            CHECK(status == CHFL_SUCCESS)
        end do

        call file%set_topology(topology, status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%read_step(int(10, int64), frame, status=status)
        CHECK(status ==0)

        atom = frame%atom(1_int64)
        CHECK(atom%name(status=status) == 'Cs')
        CHECK(status == CHFL_SUCCESS)
        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call file%close(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_set_topology_from_file()
        implicit none
        type(chfl_trajectory) :: file
        type(chfl_frame) :: frame
        type(chfl_atom) :: atom
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%open("data/trajectory.xyz", "r", status=status)
        CHECK(status == CHFL_SUCCESS)

        ! Set the topology associated with a trajectory from a file
        call file%topology_file("data/topology.xyz", "", status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%read_step(1_int64, frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        atom = frame%atom(0_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(atom%name(status=status) == 'Zn')
        CHECK(status == CHFL_SUCCESS)
        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        ! Set the topology associated with a trajectory from a file with a specific
        ! format
        call file%topology_file("data/topology.xyz", "XYZ", status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%read_step(1_int64, frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        ! call atom%from_frame(frame, 0_int64)
        ! call atom%name(name, len(name, int64), status=status)
        ! CHECK(status == CHFL_SUCCESS)
        ! CHECK(name == 'Zn')
        ! call atom%free(status=status)
        ! CHECK(status == CHFL_SUCCESS)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call file%close(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_guess_bonds()
        implicit none
        type(chfl_trajectory) :: file
        type(chfl_frame) :: frame
        type(chfl_cell) :: cell
        type(chfl_topology) :: topology
        integer :: status

        call file%open("data/water.xyz", "r", status=status)
        CHECK(status == CHFL_SUCCESS)

        call cell%init([40d0, 40d0, 40d0], status=status)
        CHECK(status == CHFL_SUCCESS)
        call file%set_cell(cell, status=status)
        CHECK(status == CHFL_SUCCESS)

        call cell%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%read(frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        ! Guess the system topology
        call frame%guess_bonds(status=status)
        CHECK(status == CHFL_SUCCESS)

        topology = frame%topology(status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%bonds_count(status=status) == 186)
        CHECK(status == CHFL_SUCCESS)

        CHECK(topology%angles_count(status=status) == 87)
        CHECK(status == CHFL_SUCCESS)

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call file%close(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_write()
        implicit none
        type(chfl_trajectory) :: file
        type(chfl_frame) :: frame

        real(real64), dimension(:, :), pointer :: positions
        character(len=2048) :: EXPECTED, content
        character :: EOL = char(10)
        integer :: status, i, j

        EXPECTED = "4" // EOL // &
                   "Written by the chemfiles library" // EOL // &
                   "X 1 2 3" // EOL // &
                   "X 1 2 3" // EOL // &
                   "X 1 2 3" // EOL // &
                   "X 1 2 3" // EOL


        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%resize(4_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        positions => frame%positions(status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(all(shape(positions) == [3, 4]))
        do i=1,3
            do j=1,4
                positions(i, j) = real(i)
            end do
        end do

        call file%open("test-tmp.xyz", "w", status=status)
        CHECK(status == CHFL_SUCCESS)
        call file%write(frame, status=status)
        CHECK(status == CHFL_SUCCESS)
        call file%close(status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        content = read_whole_file("test-tmp.xyz")
        CHECK(content == EXPECTED)

        open(unit=11, iostat=status, file="test-tmp.xyz", status='old')
        if (status == 0) close(11, status='delete')
    end subroutine

    !** Read a whole file into a string
    function read_whole_file(name) result(content)
        implicit none
        character(len=*) :: name
        character(len=2048) :: content

        integer :: status
        character(len=512) :: tmp

        content = ""
        open(file=name, unit=11, iostat=status)
        do while (.not. is_iostat_end(status))
            read(11, '(a)', iostat=status, end=999) tmp
            tmp = trim(tmp) // char(10)
            content = trim(content) // trim(tmp)
        end do
999     close(11)
    end function
end program
