#include "check.inc"

program selections_test
    use iso_fortran_env, only: int64
    use chemfiles
    implicit none

    call test_copy()
    call test_size()
    call test_string()
    call test_evaluate_atoms()
    call test_evaluate_angles()

contains
    subroutine test_copy()
        implicit none
        type(chfl_selection) :: selection, cloned
        integer :: status

        call selection%init("name O", status=status)
        CHECK(status == 0)
        call cloned%copy(selection, status=status)
        CHECK(status == 0)

        call selection%free(status=status)
        CHECK(status == 0)
        call cloned%free(status=status)
        CHECK(status == 0)
    end subroutine

    subroutine test_string()
        implicit none
        type(chfl_selection) :: selection
        character(len=32) :: string
        integer :: status

        call selection%init('name O', status=status)
        CHECK(status == 0)

        call selection%string(string, len(string, int64), status=status)
        CHECK(status == 0)
        CHECK(string == 'name O')

        call selection%free(status=status)
        CHECK(status == 0)
    end subroutine

    subroutine test_size()
        implicit none
        type(chfl_selection) :: selection
        integer(kind=int64) :: size
        integer :: status

        call selection%init("name O", status=status)
        CHECK(status == CHFL_SUCCESS)

        call selection%size(size, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(size == 1)

        call selection%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call selection%init("pairs: name(#1) Zn and name(#2) Ar", status=status)
        CHECK(status == CHFL_SUCCESS)

        call selection%size(size, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(size == 2)

        call selection%free(status=status)
        CHECK(status == CHFL_SUCCESS)
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
        CHECK(status == CHFL_SUCCESS)

        call selection%evaluate(frame, n_matches, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(n_matches == 2)

        allocate(matches(n_matches), stat=status)
        CHECK(status == CHFL_SUCCESS)

        call selection%matches(matches, n_matches, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(matches(1)%size == 1)
        CHECK(matches(2)%size == 1)
        CHECK(matches(1)%atoms(1) == 1)
        CHECK(matches(2)%atoms(1) == 2)

        deallocate(matches, stat=status)
        CHECK(status == CHFL_SUCCESS)
        call selection%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
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
        CHECK(status == CHFL_SUCCESS)

        call selection%evaluate(frame, n_matches, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(n_matches == 2)

        allocate(matches(n_matches), stat=status)
        CHECK(status == CHFL_SUCCESS)

        call selection%matches(matches, n_matches, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(matches(1)%size == 3)
        CHECK(matches(2)%size == 3)

        CHECK(matches(1)%atoms(1) == 0)
        CHECK(matches(1)%atoms(2) == 1)
        CHECK(matches(1)%atoms(3) == 2)
        CHECK(matches(2)%atoms(1) == 1)
        CHECK(matches(2)%atoms(2) == 2)
        CHECK(matches(2)%atoms(3) == 3)

        deallocate(matches, stat=status)
        CHECK(status == CHFL_SUCCESS)
        call selection%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call frame%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    function testing_frame()
        implicit none
        type(chfl_frame) :: testing_frame
        type(chfl_topology) :: topology
        type(chfl_atom) :: O, H
        integer :: status

        call O%init("O", status=status)
        CHECK(status == CHFL_SUCCESS)
        call H%init("H", status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%init(status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%add_atom(H, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_atom(O, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_atom(O, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_atom(H, status=status)
        CHECK(status == CHFL_SUCCESS)

        call O%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call H%free(status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%add_bond(0_int64, 1_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(1_int64, 2_int64, status=status)
        CHECK(status == CHFL_SUCCESS)
        call topology%add_bond(2_int64, 3_int64, status=status)
        CHECK(status == CHFL_SUCCESS)

        call testing_frame%init(status=status)
        CHECK(status == CHFL_SUCCESS)
        call testing_frame%resize(int(4, int64), status=status)
        CHECK(status == CHFL_SUCCESS)

        call testing_frame%set_topology(topology, status=status)
        CHECK(status == CHFL_SUCCESS)

        call topology%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end function
end program
