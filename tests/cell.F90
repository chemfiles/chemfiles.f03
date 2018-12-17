#include "check.inc"

program cell_test
    use iso_fortran_env, only: real64
    use chemfiles
    implicit none

    call test_copy()
    call test_triclinic()
    call test_orthorhombic()
    call test_volume()
    call test_shape()
    call test_lengths()
    call test_angles()
    call test_matrix()
    call test_wrap()

contains
    subroutine test_copy()
        implicit none
        type(chfl_cell) :: cell, cloned
        integer :: status

        call cell%init([2d0, 3d0, 4d0], status=status)
        CHECK(status == CHFL_SUCCESS)
        call cloned%copy(cell, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(all(cell%lengths(status=status) == [2.0, 3.0, 4.0]))
        CHECK(status == CHFL_SUCCESS)

        CHECK(all(cloned%lengths(status=status) == [2.0, 3.0, 4.0]))
        CHECK(status == CHFL_SUCCESS)

        call cell%set_lengths([34d0, 33d0, 35d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(all(cell%lengths(status=status) == [34.0, 33.0, 35.0]))
        CHECK(status == CHFL_SUCCESS)

        CHECK(all(cloned%lengths(status=status) == [2.0, 3.0, 4.0]))
        CHECK(status == CHFL_SUCCESS)

        call cell%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        ! Call free twice to check that it works
        call cloned%free(status=status)
        CHECK(status == CHFL_SUCCESS)
        call cloned%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_triclinic()
        implicit none
        type(chfl_cell) :: cell
        integer :: status

        call cell%init([12d0, 30d0, 24d0], [90d0, 90d0, 120d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(all(cell%lengths(status=status) == [12.0, 30.0, 24.0]))
        CHECK(status == CHFL_SUCCESS)

        CHECK(all(cell%angles(status=status) == [90.0, 90.0, 120.0]))
        CHECK(status == CHFL_SUCCESS)

        call cell%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_orthorhombic()
        implicit none
        type(chfl_cell) :: cell
        integer :: status

        call cell%init([2d0, 3d0, 4d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(all(cell%lengths(status=status) == [2.0, 3.0, 4.0]))
        CHECK(status == CHFL_SUCCESS)

        CHECK(all(cell%angles( status=status) == [90.0, 90.0, 90.0]))
        CHECK(status == CHFL_SUCCESS)

        call cell%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_volume()
        implicit none
        type(chfl_cell) :: cell
        integer :: status

        call cell%init([2d0, 3d0, 4d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(cell%volume(status=status) == 2.0 * 3.0 * 4.0)
        CHECK(status == CHFL_SUCCESS)

        call cell%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_shape()
        implicit none
        type(chfl_cell) :: cell
        integer :: status

        call cell%init([2d0, 3d0, 4d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(cell%shape(status=status) == CHFL_CELL_ORTHORHOMBIC)
        CHECK(status == CHFL_SUCCESS)

        call cell%set_shape(CHFL_CELL_TRICLINIC, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(cell%shape(status=status) == CHFL_CELL_TRICLINIC)
        CHECK(status == CHFL_SUCCESS)

        call cell%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_lengths()
        implicit none
        type(chfl_cell) :: cell
        integer :: status

        call cell%init([2d0, 3d0, 4d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(all(cell%lengths(status=status) == [2.0, 3.0, 4.0]))
        CHECK(status == CHFL_SUCCESS)

        call cell%set_lengths([10d0, 20d0, 30d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(all(cell%lengths(status=status) == [10.0, 20.0, 30.0]))
        CHECK(status == CHFL_SUCCESS)

        call cell%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_angles()
        implicit none
        type(chfl_cell) :: cell
        integer :: status

        call cell%init([2d0, 3d0, 4d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(all(cell%angles(status=status) == [90.0, 90.0, 90.0]))
        CHECK(status == CHFL_SUCCESS)

        call cell%set_angles([80d0, 89d0, 100d0], status=status)
        CHECK(status /= CHFL_SUCCESS)

        call cell%set_shape(CHFL_CELL_TRICLINIC, status=status)
        CHECK(status == CHFL_SUCCESS)

        call cell%set_angles([80d0, 89d0, 100d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(all(cell%angles(status=status) == [80.0, 89.0, 100.0]))
        CHECK(status == CHFL_SUCCESS)

        call cell%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_matrix()
        implicit none
        type(chfl_cell) :: cell
        real(real64), dimension(3, 3) :: expected, matrix
        integer :: status, i, j

        call cell%init([10d0, 20d0, 30d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        expected = reshape([10, 0, 0, &
                            0, 20, 0, &
                            0, 0, 30], [3, 3])
        matrix = cell%matrix(status=status)
        CHECK(status == CHFL_SUCCESS)
        do i=1,3
            do j=1,3
                CHECK(matrix(i, j) - expected(i, j) < 1d-10)
            end do
        end do

        call cell%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_wrap()
        implicit none
        type(chfl_cell) :: cell
        real(real64), dimension(3) :: vector
        integer :: status

        call cell%init([2d0, 3d0, 4d0], status=status)
        CHECK(status == CHFL_SUCCESS)

        vector = [0.8d0, 1.7d0, -6d0]
        call cell%wrap(vector, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(abs(vector(1) - 0.8) < 1d-6)
        CHECK(abs(vector(2) + 1.3) < 1d-6)
        CHECK(abs(vector(3) - 2) < 1d-6)

        call cell%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine
end program
