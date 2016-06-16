program trajectory_read
    use iso_fortran_env, only: real32, real64, int64
    use chemfiles
    use testing

    implicit none
    type(chfl_frame) :: frame
    type(chfl_trajectory) :: file
    type(chfl_topology) :: topology
    type(chfl_atom) :: atom
    type(chfl_cell) :: cell

    character(len=2048) :: DATADIR
    character(len=32) :: name
    integer :: status
    integer(int64) :: natoms, n, i, nsteps
    real(real32) :: pos_1(3), pos_125(3)
    real(real32), pointer :: positions(:, :)
    real(real64) :: a, b, c

    ! ================================================================================== !
    if (command_argument_count() >= 1) then
        call get_command_argument(1, DATADIR)
    else
        ! Use DATADIR as a temporary to get the program name
        call get_command_argument(0, DATADIR)
        DATADIR = "Usage: " // trim(DATADIR) // " path/to/the/DATADIRs"
        write(*, *) trim(DATADIR)
        stop 1
    end if
    ! ================================================================================== !

    call file%open(trim(DATADIR) // "/xyz/water.xyz", "r", status=status)
    call check((status == 0), "file%open")

    call file%nsteps(nsteps, status=status)
    call check((status == 0), "file%nsteps")
    call check((nsteps == 100), "file%nsteps")

    call frame%init(0_int64, status=status)
    call check((status == 0), "frame%init")

    ! Read the first frame
    call file%read(frame, status=status)
    call check((status == 0), "file%read")

    ! Check positions in the first frame
    pos_1 = [0.417219, 8.303366, 11.737172]
    pos_125 = [5.099554, -0.045104, 14.153846]
    call frame%positions(positions, natoms, status=status)
    call check((status == 0), "frame%positions")
    call check((natoms == 297), "frame%positions")

    do i=1,3
        call check((pos_1(i) == positions(i, 1)), "frame%positions")
        call check((pos_125(i) == positions(i, 125)), "frame%positions")
    end do

    ! Check topology in the first frame
    call topology%from_frame(frame, status=status)
    call check((status == 0), "topology%from_frame")

    call topology%atoms_count(natoms, status=status)
    call check((status == 0), "topology%atoms_count")
    call check((natoms == 297), "topology%atoms_count")

    call topology%bonds_count(n, status=status)
    call check((status == 0), "topology%bonds_count")
    call check((n == 0), "topology%bonds_count")

    call atom%from_topology(topology, 0_int64, status=status)
    call check((status == 0), "atom%from_topology")

    call atom%name(name, len(name, int64), status=status)
    call check((status == 0), "atom%name")
    call check((name == "O"), "atom%name")

    call atom%free(status=status)
    call check((status == 0), "atom%free")
    call topology%free(status=status)
    call check((status == 0), "topology%free")

    ! Set the cell associated with a trajectory
    call cell%init(30d0, 30d0, 30d0, status=status)
    call check((status == 0), "cell%init")

    call file%set_cell(cell, status=status)
    call check((status == 0), "trajectory%set_cell")

    call cell%free(status=status)
    call check((status == 0), "cell%free")

    ! Check reading a specific step
    call file%read_step(41_int64, frame, status=status)
    call check((status == 0), "file%read_step")

    ! Check that the cell was set
    call cell%from_frame(frame, status=status)
    call check((status == 0), "cell%from_frame")

    call cell%lengths(a, b, c, status=status)
    call check((status == 0), "cell%lengths")
    call check((a == 30d0), "cell%lengths")
    call check((b == 30d0), "cell%lengths")
    call check((c == 30d0), "cell%lengths")

    call cell%free(status=status)
    call check((status == 0), "cell%free")

    pos_1(1) = 0.761277;  pos_1(2) = 8.106125;   pos_1(3) = 10.622949;
    pos_125(1) = 5.13242; pos_125(2) = 0.079862; pos_125(3) = 14.194161;

    call frame%positions(positions, natoms, status=status)
    call check((status == 0), "frame%positions")

    do i=1,3
        call check((pos_1(i) == positions(i, 1)), "frame%positions")
        call check((pos_125(i) == positions(i, 125)), "frame%positions")
    end do

    ! Get the atom from a frame
    call atom%from_frame(frame, 1_int64);
    call atom%name(name, len(name, int64), status=status)
    call check((status == 0), "atom%name")
    call check((name == "H"), "atom%name")

    call atom%free(status=status)
    call check((status == 0), "atom%free")

    ! Guess the system topology
    call frame%guess_topology(status=status)
    call check((status == 0), "frame%guess_topology")

    call topology%from_frame(frame, status=status)
    call check((status == 0), "topology%from_frame")

    call topology%bonds_count(n, status=status)
    call check((status == 0), "topology%bonds_count")
    call check((n == 181), "topology%bonds_count")

    call topology%angles_count(n, status=status)
    call check((status == 0), "topology%angles_count")
    call check((n == 87), "topology%angles_count")

    call topology%free(status=status)
    call check((status == 0), "topology%free")

    ! Set the topology associated with a trajectory by hand
    call topology%init()
    call atom%init("Cs")

    call frame%atoms_count(natoms, status=status)
    call check((status == 0), "frame%atoms_count")
    do i=1,natoms
        call topology%append(atom, status=status)
        call check((status == 0), "topology%append")
    end do

    call file%set_topology(topology, status=status)
    call check((status == 0), "file%set_topology")

    call topology%free(status=status)
    call check((status == 0), "topology%free")

    call file%read_step(10_int64, frame, status=status)
    call check((status == 0), "file%read_step")

    call atom%from_frame(frame, 1_int64);
    call atom%name(name, len(name, int64), status=status)
    call check((status == 0), "atom%name")
    call check((name == "Cs"), "atom%name")

    call atom%free(status=status)
    call check((status == 0), "atom%free")

    call file%close(status=status)
    call check((status == 0), "file%close")
    call file%open(trim(DATADIR) // "/xyz/trajectory.xyz", 'r', status=status)
    call check((status == 0), "file%open")

    ! Set the topology associated with a trajectory from a file
    call file%set_topology_file(trim(DATADIR) // "/xyz/topology.xyz", status=status)
    call check((status == 0), "file%set_topology")
    call file%read_step(1_int64, frame, status=status)
    call check((status == 0), "file%read_step")

    call atom%from_frame(frame, 0_int64);
    call atom%name(name, len(name, int64), status=status)
    call check((status == 0), "atom%name")
    call check((name == "Zn"), "atom%name")

    call atom%free(status=status)
    call check((status == 0), "atom%free")
    call file%close(status=status)
    call check((status == 0), "file%close")

    call file%with_format(trim(DATADIR) // "/xyz/helium.xyz.but.not.really", "r", "XYZ", status=status)
    call check((status == 0), "file%open_with_format")

    call file%read(frame, status=status)
    call check((status == 0), "file%read")

    call frame%atoms_count(natoms, status=status)
    call check((status == 0), "frame%atoms_count")
    call check((natoms == 125), "frame%atoms_count")

    call frame%free(status=status)
    call check((status == 0), "frame%free")
    call file%close(status=status)
    call check((status == 0), "file%close")
end program
