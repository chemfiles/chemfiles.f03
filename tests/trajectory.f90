program trajectory_read
    use iso_fortran_env, only: real64, int64
    use chemfiles
    use testing
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
        call check(status == CHFL_SUCCESS, "file%open")

        call file%nsteps(nsteps, status=status)
        call check(status == CHFL_SUCCESS, "file%nsteps")
        call check(nsteps == 100, "file%nsteps")

        call frame%init(status=status)
        call check(status == CHFL_SUCCESS, "frame%init")

        ! Read the first frame
        call file%read(frame, status=status)
        call check(status == CHFL_SUCCESS, "file%read")

        ! Check positions in the first frame
        pos_1 = [0.417219, 8.303366, 11.737172]
        pos_125 = [5.099554, -0.045104, 14.153846]
        call frame%positions(positions, natoms, status=status)
        call check(status == CHFL_SUCCESS, "frame%positions")
        call check(natoms == 297, "frame%positions")

        do i=1,3
            call check(abs(pos_1(i) - positions(i, 1)) < 1d-6, "frame%positions")
            call check(abs(pos_125(i) - positions(i, 125)) < 1d-6, "frame%positions")
        end do

        ! Check reading a specific step
        call file%read_step(int(41, int64), frame, status=status)
        call check(status == CHFL_SUCCESS, "file%read_step")

        pos_1(1) = 0.761277;  pos_1(2) = 8.106125;   pos_1(3) = 10.622949;
        pos_125(1) = 5.13242; pos_125(2) = 0.079862; pos_125(3) = 14.194161;

        call frame%positions(positions, natoms, status=status)
        call check(status == CHFL_SUCCESS, "frame%positions")

        do i=1,3
            call check(abs(pos_1(i) - positions(i, 1)) < 1d-6, "frame%positions")
            call check(abs(pos_125(i) - positions(i, 125)) < 1d-6, "frame%positions")
        end do

        call frame%free(status=status)
        call check(status == CHFL_SUCCESS, "frame%free")
        call file%close(status=status)
        call check(status == CHFL_SUCCESS, "file%close")
    end subroutine

    subroutine test_open_with_format()
        implicit none
        type(chfl_trajectory) :: file
        type(chfl_frame) :: frame
        integer(int64) :: natoms
        integer :: status

        call frame%init(status=status)
        call check(status == CHFL_SUCCESS, "frame%init")

        call file%with_format(trim(DATADIR) // "/xyz/helium.xyz.but.not.really", "r", "XYZ", status=status)
        call check(status == CHFL_SUCCESS, "file%open_with_format")

        call file%read(frame, status=status)
        call check(status == CHFL_SUCCESS, "file%read")

        call frame%atoms_count(natoms, status=status)
        call check(status == CHFL_SUCCESS, "frame%atoms_count")
        call check(natoms == 125, "frame%atoms_count")

        call frame%free(status=status)
        call check(status == CHFL_SUCCESS, "frame%free")
        call file%close(status=status)
        call check(status == CHFL_SUCCESS, "file%close")
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
        call check(status == CHFL_SUCCESS, "cell%init")

        call file%open(trim(DATADIR) // "/xyz/water.xyz", "r", status=status)
        call check(status == CHFL_SUCCESS, "file%open")

        call file%set_cell(cell, status=status)
        call check(status == CHFL_SUCCESS, "trajectory%set_cell")

        call cell%free(status=status)
        call check(status == CHFL_SUCCESS, "cell%free")

        call frame%init(status=status)
        call check(status == CHFL_SUCCESS, "frame%init")

        call file%read(frame, status=status)
        call check(status == CHFL_SUCCESS, "file%read")

        ! Check that the cell was set
        call cell%from_frame(frame, status=status)
        call check(status == CHFL_SUCCESS, "cell%from_frame")

        call cell%lengths(lengths, status=status)
        call check(status == CHFL_SUCCESS, "cell%lengths")
        call check(all(lengths == [30.0, 30.0, 30.0]), "cell%lengths")

        call frame%free(status=status)
        call check(status == CHFL_SUCCESS, "frame%free")
        call cell%free(status=status)
        call check(status == CHFL_SUCCESS, "cell%free")
        call file%close(status=status)
        call check(status == CHFL_SUCCESS, "file%close")
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
        call check(status == CHFL_SUCCESS, "file%open")

        ! Set the topology associated with a trajectory by hand
        call topology%init()
        call atom%init("Cs")

        do i=1,297
            call topology%add_atom(atom, status=status)
            call check(status == CHFL_SUCCESS, "topology%append")
        end do

        call file%set_topology(topology, status=status)
        call check(status == CHFL_SUCCESS, "file%set_topology")

        call atom%free(status=status)
        call check(status == CHFL_SUCCESS, "atom%free")
        call topology%free(status=status)
        call check(status == CHFL_SUCCESS, "topology%free")

        call frame%init(status=status)
        call check(status == CHFL_SUCCESS, "frame%init")

        call file%read_step(int(10, int64), frame, status=status)
        call check(status ==0, "file%read_step")

        call atom%from_frame(frame, int(1, int64))
        call atom%name(name, len(name, int64), status=status)
        call check(status == CHFL_SUCCESS, "atom%name")
        call check(name == "Cs", "atom%name")

        call frame%free(status=status)
        call check(status == CHFL_SUCCESS, "frame%free")
        call file%close(status=status)
        call check(status == CHFL_SUCCESS, "file%close")
    end subroutine

    subroutine test_set_topology_from_file()
        implicit none
        type(chfl_trajectory) :: file
        type(chfl_frame) :: frame
        type(chfl_atom) :: atom
        character(len=32) :: name
        integer :: status

        call frame%init(status=status)
        call check(status == CHFL_SUCCESS, "frame%init")

        call file%open(trim(DATADIR) // "/xyz/trajectory.xyz", "r", status=status)
        call check(status == CHFL_SUCCESS, "file%open")

        ! Set the topology associated with a trajectory from a file
        call file%topology_file(trim(DATADIR) // "/xyz/topology.xyz", "", status=status)
        call check(status == CHFL_SUCCESS, "file%topology_file")

        call file%read_step(int(1, int64), frame, status=status)
        call check(status == CHFL_SUCCESS, "file%read_step")

        call atom%from_frame(frame, int(0, int64));
        call atom%name(name, len(name, int64), status=status)
        call check(status == CHFL_SUCCESS, "atom%name")
        call check(name == "Zn", "atom%name")

        call atom%free(status=status)
        call check(status == CHFL_SUCCESS, "atom%free")

        ! Set the topology associated with a trajectory from a file with a specific
        ! format
        call file%topology_file(trim(DATADIR) // "/xyz/topology.xyz", "XYZ", status=status)
        call check(status == CHFL_SUCCESS, "file%set_topology_with_format")

        call file%read_step(int(1, int64), frame, status=status)
        call check(status == CHFL_SUCCESS, "file%read_step")

        call atom%from_frame(frame, int(0, int64));
        call atom%name(name, len(name, int64), status=status)
        call check(status == CHFL_SUCCESS, "atom%name")
        call check(name == "Zn", "atom%name")

        call atom%free(status=status)
        call check(status == CHFL_SUCCESS, "atom%free")
        call frame%free(status=status)
        call check(status == CHFL_SUCCESS, "frame%free")
        call file%close(status=status)
        call check(status == CHFL_SUCCESS, "file%close")
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
        call check(status == CHFL_SUCCESS, "file%open")

        call cell%init([40d0, 40d0, 40d0], status=status)
        call check(status == CHFL_SUCCESS, "cell%init")
        call file%set_cell(cell, status=status)
        call check(status == CHFL_SUCCESS, "file%set_cell")

        call cell%free(status=status)
        call check(status == CHFL_SUCCESS, "cell%free")

        call frame%init(status=status)
        call check(status == CHFL_SUCCESS, "frame%init")

        call file%read(frame, status=status)
        call check(status == CHFL_SUCCESS, "file%read")

        ! Guess the system topology
        call frame%guess_topology(status=status)
        call check(status == CHFL_SUCCESS, "frame%guess_topology")

        call topology%from_frame(frame, status=status)
        call check(status == CHFL_SUCCESS, "topology%from_frame")

        call topology%bonds_count(n, status=status)
        call check(status == CHFL_SUCCESS, "topology%bonds_count")
        call check(n == 186, "topology%bonds_count")

        call topology%angles_count(n, status=status)
        call check(status == CHFL_SUCCESS, "topology%angles_count")
        call check(n == 87, "topology%angles_count")

        call topology%free(status=status)
        call check(status == CHFL_SUCCESS, "topology%free")
        call frame%free(status=status)
        call check(status == CHFL_SUCCESS, "frame%free")
        call file%close(status=status)
        call check(status == CHFL_SUCCESS, "file%close")
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
        call check(status == CHFL_SUCCESS, "frame%init")
        call frame%resize(int(4, int64), status=status)
        call check(status == CHFL_SUCCESS, "frame%resize")

        call frame%positions(positions, natoms, status=status)
        call check(status == CHFL_SUCCESS, "frame%positions")
        call check(natoms == 4, "frame%positions")
        do i=1,3
            do j=1,4
                positions(i, j) = real(i)
            end do
        end do

        call file%open("test-tmp.xyz", "w", status=status)
        call check(status == CHFL_SUCCESS, "file%open")
        call file%write(frame, status=status)
        call check(status == CHFL_SUCCESS, "file%write")
        call file%close(status=status)
        call check(status == CHFL_SUCCESS, "file%close")

        content = read_whole_file("test-tmp.xyz")
        call check(content == EXPECTED, "Check file content")

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
