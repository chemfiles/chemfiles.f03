program selections_test
    use iso_fortran_env, only: int64
    use chemfiles
    use testing

    implicit none
    type(chfl_frame) :: frame
    type(chfl_selection) :: selection
    type(chfl_match), dimension(:), allocatable :: matches
    integer(kind=int64) :: size, n_matches
    integer :: status

    frame = testing_frame()
    call selection%init("name O", status=status)
    call check(status == 0, "selection%init")

    call selection%size(size, status=status)
    call check(status == 0, "selection%size")
    call check(size == 1, "selection%size")

    call selection%evaluate(frame, n_matches, status=status)
    call check(status == 0, "selection%evalutate")
    call check(n_matches == 2, "selection%size")

    allocate(matches(n_matches), stat=status)
    call check(status == 0, "allocate")

    call selection%matches(matches, n_matches, status=status)
    call check(status == 0, "selection%matches")

    call check(matches(1)%size == 1, "matches")
    call check(matches(2)%size == 1, "matches")
    call check(matches(1)%atoms(1) == 1, "matches")
    call check(matches(2)%atoms(1) == 2, "matches")

    deallocate(matches, stat=status)
    call check(status == 0, "deallocate")
    call selection%free(status=status)
    call check(status == 0, "selection%free")

    ! ****************************************************************  !

    call selection%init("not index <= 2", status=status)
    call check(status == 0, "selection%init")

    call selection%size(size, status=status)
    call check(status == 0, "selection%size")
    call check(size == 1, "selection%size")

    call selection%evaluate(frame, n_matches, status=status)
    call check(status == 0, "selection%evalutate")
    call check(n_matches == 1, "selection%size")

    allocate(matches(n_matches), stat=status)
    call check(status == 0, "allocate")

    call selection%matches(matches, n_matches, status=status)
    call check(status == 0, "selection%matches")

    call check(matches(1)%size == 1, "matches")
    call check(matches(1)%atoms(1) == 3, "matches")

    deallocate(matches, stat=status)
    call check(status == 0, "deallocate")
    call selection%free(status=status)
    call check(status == 0, "selection%free")

    ! ****************************************************************  !

    call selection%init("angles: all", status=status)
    call check(status == 0, "selection%init")

    call selection%size(size, status=status)
    call check(status == 0, "selection%size")
    call check(size == 3, "selection%size")

    call selection%evaluate(frame, n_matches, status=status)
    call check(status == 0, "selection%evalutate")
    call check(n_matches == 2, "selection%size")

    allocate(matches(n_matches), stat=status)
    call check(status == 0, "allocate")

    call selection%matches(matches, n_matches, status=status)
    call check(status == 0, "selection%matches")

    call check(matches(1)%size == 3, "matches")
    call check(matches(2)%size == 3, "matches")

    call check(find_match(matches, (/0, 1, 2/)), "find_match")
    call check(find_match(matches, (/1, 2, 3/)), "find_match")

    deallocate(matches, stat=status)
    call check(status == 0, "deallocate")
    call selection%free(status=status)
    call check(status == 0, "selection%free")

    call frame%free(status=status)
    call check(status == 0, "frame%free")

contains
    function testing_frame()
        implicit none
        type(chfl_frame) :: testing_frame
        type(chfl_topology) :: topology
        type(chfl_atom) :: O, H
        integer :: status

        call O%init("O", status=status)
        call check(status == 0, "O%init")
        call H%init("H", status=status)
        call check(status == 0, "H%init")
        call topology%init(status=status)
        call check(status == 0, "topology%init")

        call topology%add_atom(H, status=status)
        call check(status == 0, "topology%append")
        call topology%add_atom(O, status=status)
        call check(status == 0, "topology%append")
        call topology%add_atom(O, status=status)
        call check(status == 0, "topology%append")
        call topology%add_atom(H, status=status)
        call check(status == 0, "topology%append")

        call O%free(status=status)
        call check(status == 0, "O%free")
        call H%free(status=status)
        call check(status == 0, "H%free")

        call topology%add_bond(0_int64, 1_int64, status=status)
        call check(status == 0, "topology%add_bond")
        call topology%add_bond(1_int64, 2_int64, status=status)
        call check(status == 0, "topology%add_bond")
        call topology%add_bond(2_int64, 3_int64, status=status)
        call check(status == 0, "topology%add_bond")

        call testing_frame%init(status=status)
        call check(status == 0, "testing_frame%init")
        call testing_frame%resize(int(4, int64), status=status)
        call check(status == 0, "testing_frame%resize")

        call testing_frame%set_topology(topology, status=status)
        call check(status == 0, "testing_frame%set_topology")

        call topology%free(status=status)
        call check(status == 0, "topology%free")
    end function

    function find_match(matches, match)
        type(chfl_match), allocatable, intent(in) :: matches(:)
        integer, dimension(3), intent(in) :: match
        logical :: find_match
        integer :: i

        find_match = .false.
        do i=1,2 ! FIXME: using 'size(matches)' does not compile
            if  (matches(i)%atoms(1) == match(1) .and. &
                 matches(i)%atoms(2) == match(2) .and.  &
                 matches(i)%atoms(3) == match(3)) then
               find_match = .true.
               return
           endif
        enddo
    end function
end program
