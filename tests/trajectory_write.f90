PROGRAM trajectory_read
    use iso_fortran_env, only: real32, real64, int64
    use chemfiles
    use testing

    implicit none
    type(chfl_trajectory) :: file
    type(chfl_topology) :: topology
    type(chfl_atom) :: atom
    type(chfl_frame) :: frame

    real(real32), dimension(:, :), pointer :: positions
    integer(int64) :: natoms
    character(len=2048) :: expected_content, content
    character :: EOL = char(10)
    integer :: status, i, j

    expected_content = "4" // EOL // &
                       "Written by the chemfiles library" // EOL // &
                       "He 1 2 3" // EOL // &
                       "He 1 2 3" // EOL // &
                       "He 1 2 3" // EOL // &
                       "He 1 2 3" // EOL // &
                       "6" // EOL // &
                       "Written by the chemfiles library" // EOL // &
                       "He 4 5 6" // EOL // &
                       "He 4 5 6" // EOL // &
                       "He 4 5 6" // EOL // &
                       "He 4 5 6" // EOL // &
                       "He 4 5 6" // EOL // &
                       "He 4 5 6" // EOL

    call topology%init(status=status)
    call check((status == 0), "topology%init")
    call atom%init("He", status=status)
    call check((status == 0), "atom%init")

    do i=1,4
        call topology%append(atom, status=status)
        call check((status == 0), "topology%append")
    end do

    call frame%init(0_int64, status=status)
    call check((status == 0), "frame%init")
    call frame%resize(4_int64, status=status)
    call check((status == 0), "frame%resize")
    call frame%positions(positions, natoms, status=status)
    call check((status == 0), "frame%positions")
    call check((natoms == 4), "frame%positions")
    do i=1,3
        do j=1,4
            positions(i, j) = real(i)
        end do
    end do

    call frame%set_topology(topology, status=status)
    call check((status == 0), "frame%set_topology")

    call file%open("test-tmp.xyz", "w", status=status)
    call check((status == 0), "file%open")
    call file%write(frame, status=status)
    call check((status == 0), "file%write")

    call topology%append(atom, status=status)
    call check((status == 0), "topology%append")
    call topology%append(atom, status=status)
    call check((status == 0), "topology%append")

    call frame%resize(6_int64, status=status)
    call check((status == 0), "frame%resize")
    call frame%positions(positions, natoms, status=status)
    call check((status == 0), "frame%positions")
    call check((natoms == 6), "frame%positions")

    do i=1,3
        do j=1,6
            positions(i, j) = real(i + 3)
        end do
    end do

    call frame%set_topology(topology, status=status)
    call check((status == 0), "frame%set_topology")

    call atom%free(status=status)
    call check((status == 0), "atom%free")
    call topology%free(status=status)
    call check((status == 0), "topology%free")

    call file%write(frame, status=status)
    call check((status == 0), "file%write")
    call file%sync(status=status)
    call check((status == 0), "file%sync")


    content = read_whole_file("test-tmp.xyz")
    call check((content == expected_content), "Check file content")

    open(unit=11, iostat=status, file="test-tmp.xyz", status='old')
    if (status == 0) close(11, status='delete')
contains
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
