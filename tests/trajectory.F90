#include "check.inc"

program trajectory_read
    use iso_fortran_env, only: real64, int64
    use chemfiles
    implicit none

    character(len=2048) :: DATADIR
    if (command_argument_count() >= 1) then
        call get_command_argument(1, DATADIR)
    else
        ! Use DATADIR as a temporary to get the program name
        call get_command_argument(0, DATADIR)
        DATADIR = "Usage: " // trim(DATADIR) // " path/to/the/DATADIRs"
        write(*, *) trim(DATADIR)
        stop 1
    end if

    call test_read()
    call test_open_with_format()
    call test_set_cell()
    call test_set_topology()
    call test_set_topology_from_file()
    call test_guess_topology()
    call test_write()

contains
    subroutine test_read()
        implicit none
        type(chfl_trajectory) :: file
        type(chfl_frame) :: frame
        real(real64) :: pos_1(3), pos_125(3)
        real(real64), pointer :: positions(:, :)
        integer(int64) :: natoms, nsteps, i
        integer :: status

        call file%open(trim(DATADIR) // "/xyz/water.xyz", "r", status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%nsteps(nsteps, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(nsteps == 100)

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        ! Read the first frame
        call file%read(frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        ! Check positions in the first frame
        pos_1 = [0.417219, 8.303366, 11.737172]
        pos_125 = [5.099554, -0.045104, 14.153846]
        call frame%positions(positions, natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 297)

        do i=1,3
            CHECK(abs(pos_1(i) - positions(i, 1)) < 1d-6)
            CHECK(abs(pos_125(i) - positions(i, 125)) < 1d-6)
        end do

        ! Check reading a specific step
        call file%read_step(int(41, int64), frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        pos_1(1) = 0.761277;  pos_1(2) = 8.106125;   pos_1(3) = 10.622949;
        pos_125(1) = 5.13242; pos_125(2) = 0.079862; pos_125(3) = 14.194161;

        call frame%positions(positions, natoms, status=status)
        CHECK(status == CHFL_SUCCESS)

        do i=1,3
            CHECK(abs(pos_1(i) - positions(i, 1)) < 1d-6)
            CHECK(abs(pos_125(i) - positions(i, 125)) < 1d-6)
        end do

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call file%close(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_open_with_format()
        implicit none
        type(chfl_trajectory) :: file
        type(chfl_frame) :: frame
        integer(int64) :: natoms
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%with_format(trim(DATADIR) // "/xyz/helium.xyz.but.not.really", "r", "XYZ", status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%read(frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%atoms_count(natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 125)

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
        real(real64), dimension(3) :: lengths
        integer :: status

        ! Set the cell associated with a trajectory
        call cell%init([30d0, 30d0, 30d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%open(trim(DATADIR) // "/xyz/water.xyz", "r", status=status)
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
        call cell%from_frame(frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        call cell%lengths(lengths, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(all(lengths == [30.0, 30.0, 30.0]))

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
        character(len=32) :: name
        integer :: status, i

        call file%open(trim(DATADIR) // "/xyz/water.xyz", "r", status=status)
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

        call atom%from_frame(frame, int(1, int64))
        call atom%name(name, len(name, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(name == 'Cs')

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call file%close(status=status)
        CHECK(status == CHFL_SUCCESS)
        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_set_topology_from_file()
        implicit none
        type(chfl_trajectory) :: file
        type(chfl_frame) :: frame
        type(chfl_atom) :: atom
        character(len=32) :: name
        integer :: status

        call frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%open(trim(DATADIR) // "/xyz/trajectory.xyz", "r", status=status)
        CHECK(status == CHFL_SUCCESS)

        ! Set the topology associated with a trajectory from a file
        call file%topology_file(trim(DATADIR) // "/xyz/topology.xyz", "", status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%read_step(int(1, int64), frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%from_frame(frame, int(0, int64));
        call atom%name(name, len(name, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(name == 'Zn')

        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        ! Set the topology associated with a trajectory from a file with a specific
        ! format
        call file%topology_file(trim(DATADIR) // "/xyz/topology.xyz", "XYZ", status=status)
        CHECK(status == CHFL_SUCCESS)

        call file%read_step(int(1, int64), frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%from_frame(frame, int(0, int64));
        call atom%name(name, len(name, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(name == 'Zn')

        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call file%close(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_guess_topology()
        implicit none
        type(chfl_trajectory) :: file
        type(chfl_frame) :: frame
        type(chfl_cell) :: cell
        type(chfl_topology) :: topology
        integer(int64) :: n
        integer :: status

        call file%open(trim(DATADIR) // "/xyz/water.xyz", "r", status=status)
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
        call frame%guess_topology(status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%from_frame(frame, status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%bonds_count(n, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(n == 186)

        call topology%angles_count(n, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(n == 87)

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
        integer(int64) :: natoms
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
        call frame%resize(int(4, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%positions(positions, natoms, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(natoms == 4)
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
