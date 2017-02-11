program selections_test
    use iso_fortran_env, only: int64
    use chemfiles
    use testing
    implicit none

    call test_size()
    call test_evaluate_atoms()
    call test_evaluate_angles()

contains
    subroutine test_size()
        implicit none
        type(chfl_selection) :: selection
        integer(kind=int64) :: size
        integer :: status

        call selection%init("name O", status=status)
        call check(status == CHFL_SUCCESS, "selection%init")

        call selection%size(size, status=status)
        call check(status == CHFL_SUCCESS, "selection%size")
        call check(size == 1, "selection%size")

        call selection%free(status=status)
        call check(status == CHFL_SUCCESS, "selection%free")

        call selection%init("pairs: name(#1) Zn and name(#2) Ar", status=status)
        call check(status == CHFL_SUCCESS, "selection%init")

        call selection%size(size, status=status)
        call check(status == CHFL_SUCCESS, "selection%size")
        call check(size == 2, "selection%size")

        call selection%free(status=status)
        call check(status == CHFL_SUCCESS, "selection%free")
    end subroutine

    subroutine test_evaluate_atoms()
        implicit none
        type(chfl_frame) :: frame
        type(chfl_selection) :: selection
        type(chfl_match), dimension(:), allocatable :: matches
        integer(kind=int64) :: n_matches
        integer :: status

        frame = testing_frame()

        call selection%init("name O", status=status)
        call check(status == CHFL_SUCCESS, "selection%init")

        call selection%evaluate(frame, n_matches, status=status)
        call check(status == CHFL_SUCCESS, "selection%evalutate")
        call check(n_matches == 2, "selection%size")

        allocate(matches(n_matches), stat=status)
        call check(status == CHFL_SUCCESS, "allocate")

        call selection%matches(matches, n_matches, status=status)
        call check(status == CHFL_SUCCESS, "selection%matches")

        call check(matches(1)%size == 1, "matches")
        call check(matches(2)%size == 1, "matches")
        call check(matches(1)%atoms(1) == 1, "matches")
        call check(matches(2)%atoms(1) == 2, "matches")

        deallocate(matches, stat=status)
        call check(status == CHFL_SUCCESS, "deallocate")
        call selection%free(status=status)
        call check(status == CHFL_SUCCESS, "selection%free")

        call frame%free(status=status)
        call check(status == CHFL_SUCCESS, "frame%free")
    end subroutine

    subroutine test_evaluate_angles()
        implicit none
        type(chfl_frame) :: frame
        type(chfl_selection) :: selection
        type(chfl_match), dimension(:), allocatable :: matches
        integer(kind=int64) :: n_matches
        integer :: status

        frame = testing_frame()

        call selection%init("angles: all", status=status)
        call check(status == CHFL_SUCCESS, "selection%init")

        call selection%evaluate(frame, n_matches, status=status)
        call check(status == CHFL_SUCCESS, "selection%evalutate")
        call check(n_matches == 2, "selection%size")

        allocate(matches(n_matches), stat=status)
        call check(status == CHFL_SUCCESS, "allocate")

        call selection%matches(matches, n_matches, status=status)
        call check(status == CHFL_SUCCESS, "selection%matches")

        call check(matches(1)%size == 3, "matches")
        call check(matches(2)%size == 3, "matches")

        call check(matches(1)%atoms(1) == 0, "matches")
        call check(matches(1)%atoms(2) == 1, "matches")
        call check(matches(1)%atoms(3) == 2, "matches")
        call check(matches(2)%atoms(1) == 1, "matches")
        call check(matches(2)%atoms(2) == 2, "matches")
        call check(matches(2)%atoms(3) == 3, "matches")

        deallocate(matches, stat=status)
        call check(status == CHFL_SUCCESS, "deallocate")
        call selection%free(status=status)
        call check(status == CHFL_SUCCESS, "selection%free")

        call frame%free(status=status)
        call check(status == CHFL_SUCCESS, "frame%free")
    end subroutine

    function testing_frame()
        implicit none
        type(chfl_frame) :: testing_frame
        type(chfl_topology) :: topology
        type(chfl_atom) :: O, H
        integer :: status

        call O%init("O", status=status)
        call check(status == CHFL_SUCCESS, "O%init")
        call H%init("H", status=status)
        call check(status == CHFL_SUCCESS, "H%init")
        call topology%init(status=status)
        call check(status == CHFL_SUCCESS, "topology%init")

        call topology%add_atom(H, status=status)
        call check(status == CHFL_SUCCESS, "topology%append")
        call topology%add_atom(O, status=status)
        call check(status == CHFL_SUCCESS, "topology%append")
        call topology%add_atom(O, status=status)
        call check(status == CHFL_SUCCESS, "topology%append")
        call topology%add_atom(H, status=status)
        call check(status == CHFL_SUCCESS, "topology%append")

        call O%free(status=status)
        call check(status == CHFL_SUCCESS, "O%free")
        call H%free(status=status)
        call check(status == CHFL_SUCCESS, "H%free")

        call topology%add_bond(0_int64, 1_int64, status=status)
        call check(status == CHFL_SUCCESS, "topology%add_bond")
        call topology%add_bond(1_int64, 2_int64, status=status)
        call check(status == CHFL_SUCCESS, "topology%add_bond")
        call topology%add_bond(2_int64, 3_int64, status=status)
        call check(status == CHFL_SUCCESS, "topology%add_bond")

        call testing_frame%init(status=status)
        call check(status == CHFL_SUCCESS, "testing_frame%init")
        call testing_frame%resize(int(4, int64), status=status)
        call check(status == CHFL_SUCCESS, "testing_frame%resize")

        call testing_frame%set_topology(topology, status=status)
        call check(status == CHFL_SUCCESS, "testing_frame%set_topology")

        call topology%free(status=status)
        call check(status == CHFL_SUCCESS, "topology%free")
    end function
end program
