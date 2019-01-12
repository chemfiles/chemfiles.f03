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
        CHECK(status == CHFL_SUCCESS)
        call cloned%copy(selection, status=status)
        CHECK(status == CHFL_SUCCESS)

        call selection%free()
        ! Call free twice to check that it works
        call cloned%free()
        call cloned%free()
    end subroutine

    subroutine test_string()
        implicit none
        type(chfl_selection) :: selection
        integer :: status

        call selection%init('name O', status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(selection%string(status=status) == 'name O')
        CHECK(status == CHFL_SUCCESS)

        call selection%free()
    end subroutine

    subroutine test_size()
        implicit none
        type(chfl_selection) :: selection
        integer :: status

        call selection%init("name O", status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(selection%size(status=status) == 1)
        CHECK(status == CHFL_SUCCESS)

        call selection%free()

        call selection%init("pairs: name(#1) Zn and name(#2) Ar", status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(selection%size(status=status) == 2)
        CHECK(status == CHFL_SUCCESS)

        call selection%free()
    end subroutine

    subroutine test_evaluate_atoms()
        implicit none
        type(chfl_frame) :: frame
        type(chfl_selection) :: selection
        type(chfl_match), dimension(:), allocatable :: matches
        integer(kind=int64) :: count
        integer :: status

        frame = testing_frame()

        call selection%init("name O", status=status)
        CHECK(status == CHFL_SUCCESS)

        call selection%evaluate(frame, count, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(count == 2)

        allocate(matches(count), stat=status)
        CHECK(status == CHFL_SUCCESS)

        call selection%matches(matches, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(matches(1)%size == 1)
        CHECK(matches(2)%size == 1)
        CHECK(matches(1)%atoms(1) == 1)
        CHECK(matches(2)%atoms(1) == 2)

        deallocate(matches, stat=status)
        CHECK(status == CHFL_SUCCESS)
        call selection%free()

        call frame%free()
    end subroutine

    subroutine test_evaluate_angles()
        implicit none
        type(chfl_frame) :: frame
        type(chfl_selection) :: selection
        type(chfl_match), dimension(:), allocatable :: matches
        integer(kind=int64) :: count
        integer :: status

        frame = testing_frame()

        call selection%init("angles: all", status=status)
        CHECK(status == CHFL_SUCCESS)

        call selection%evaluate(frame, count, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(count == 2)

        allocate(matches(count), stat=status)
        CHECK(status == CHFL_SUCCESS)

        call selection%matches(matches, status=status)
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
        call selection%free()

        call frame%free()
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

        call O%free()
        call H%free()

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

        call topology%free()
    end function
end program
