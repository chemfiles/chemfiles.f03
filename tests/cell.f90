program cell_test
    use iso_fortran_env, only: real64
    use chemfiles
    use testing
    implicit none

    call test_copy()
    call test_triclinic()
    call test_orthorhombic()
    call test_volume()
    call test_shape()
    call test_lengths()
    call test_angles()
    call test_matrix()

contains
    subroutine test_copy()
        implicit none
        type(chfl_cell) :: cell, cloned
        real(real64), dimension(3) :: lengths
        integer :: status

        call cell%init([2d0, 3d0, 4d0], status=status)
        call check(status == 0, "cell%init")
        call cloned%copy(cell, status=status)
        call check(status == 0, "cell%copy")

        call cell%lengths(lengths, status=status)
        call check(status == 0, "cell%lengths")
        call check(all(lengths == [2.0, 3.0, 4.0]), "cell%lengths")
        call cloned%lengths(lengths, status=status)
        call check(status == 0, "cell%lengths")
        call check(all(lengths == [2.0, 3.0, 4.0]), "cell%lengths")

        call cell%set_lengths([34d0, 33d0, 35d0], status=status)
        call check(status == 0, "cell%set_lengths")

        call cell%lengths(lengths, status=status)
        call check(status == 0, "cell%lengths")
        call check(all(lengths == [34.0, 33.0, 35.0]), "cell%lengths")
        call cloned%lengths(lengths, status=status)
        call check(status == 0, "cell%lengths")
        call check(all(lengths == [2.0, 3.0, 4.0]), "cell%lengths")

        call cell%free(status=status)
        call check(status == 0, "cell%free")
        call cloned%free(status=status)
        call check(status == 0, "cell%free")
    end subroutine

    subroutine test_triclinic()
        implicit none
        type(chfl_cell) :: cell
        real(real64), dimension(3) :: lengths, angles
        integer :: status

        call cell%triclinic([12d0, 30d0, 24d0], [90d0, 90d0, 120d0], status=status)
        call check(status == CHFL_SUCCESS, "cell%triclinic")

        call cell%lengths(lengths, status=status)
        call check(status == CHFL_SUCCESS, "cell%lengths")
        call check(all(lengths == [12.0, 30.0, 24.0]), "cell%lengths")

        call cell%angles(angles, status=status)
        call check(status == CHFL_SUCCESS, "cell%angles")
        call check(all(angles == [90.0, 90.0, 120.0]), "cell%angles")

        call cell%free(status=status)
        call check(status == CHFL_SUCCESS, "cell%free")
    end subroutine

    subroutine test_orthorhombic()
        implicit none
        type(chfl_cell) :: cell
        real(real64), dimension(3) :: lengths, angles
        integer :: status

        call cell%init([2d0, 3d0, 4d0], status=status)
        call check(status == CHFL_SUCCESS, "cell%init")

        call cell%lengths(lengths, status=status)
        call check(status == CHFL_SUCCESS, "cell%lengths")
        call check(all(lengths == [2.0, 3.0, 4.0]), "cell%lengths")

        call cell%angles(angles, status=status)
        call check(status == CHFL_SUCCESS, "cell%angles")
        call check(all(angles == [90.0, 90.0, 90.0]), "cell%angles")

        call cell%free(status=status)
        call check(status == CHFL_SUCCESS, "cell%free")
    end subroutine

    subroutine test_volume()
        implicit none
        type(chfl_cell) :: cell
        real(real64) :: volume = 0.0
        integer :: status

        call cell%init([2d0, 3d0, 4d0], status=status)
        call check(status == CHFL_SUCCESS, "cell%init")

        call cell%volume(volume, status=status)
        call check(status == CHFL_SUCCESS, "cell%volume")
        call check(volume == 2.0*3.0*4.0, "cell%volume")

        call cell%free(status=status)
        call check(status == CHFL_SUCCESS, "cell%free")
    end subroutine

    subroutine test_shape()
        implicit none
        type(chfl_cell) :: cell
        integer(chfl_cell_shape_t) :: shape
        integer :: status

        call cell%init([2d0, 3d0, 4d0], status=status)
        call check(status == CHFL_SUCCESS, "cell%init")

        call cell%shape(shape, status=status)
        call check(status == CHFL_SUCCESS, "cell%type")
        call check(shape == CHFL_CELL_ORTHORHOMBIC, "cell%type")

        call cell%set_shape(CHFL_CELL_TRICLINIC, status=status)
        call check(status == CHFL_SUCCESS, "cell%set_type")
        call cell%shape(shape, status=status)
        call check(status == CHFL_SUCCESS, "cell%type")
        call check(shape == CHFL_CELL_TRICLINIC, "cell%type")

        call cell%free(status=status)
        call check(status == CHFL_SUCCESS, "cell%free")
    end subroutine

    subroutine test_lengths()
        implicit none
        type(chfl_cell) :: cell
        real(real64), dimension(3) :: lengths
        integer :: status

        call cell%init([2d0, 3d0, 4d0], status=status)
        call check(status == CHFL_SUCCESS, "cell%init")

        call cell%lengths(lengths, status=status)
        call check(status == CHFL_SUCCESS, "cell%lengths")
        call check(all(lengths == [2.0, 3.0, 4.0]), "cell%lengths")

        call cell%set_lengths([10d0, 20d0, 30d0], status=status)
        call check(status == CHFL_SUCCESS, "cell%set_lengths")

        call cell%lengths(lengths, status=status)
        call check(status == CHFL_SUCCESS, "cell%lengths")
        call check(all(lengths == [10.0, 20.0, 30.0]), "cell%lengths")

        call cell%free(status=status)
        call check(status == CHFL_SUCCESS, "cell%free")
    end subroutine

    subroutine test_angles()
        implicit none
        type(chfl_cell) :: cell
        real(real64), dimension(3) :: angles
        integer :: status

        call cell%init([2d0, 3d0, 4d0], status=status)
        call check(status == CHFL_SUCCESS, "cell%init")

        call cell%set_angles([80d0, 89d0, 100d0], status=status)
        call check(status /= CHFL_SUCCESS, "cell%set_angles")

        call cell%set_shape(CHFL_CELL_TRICLINIC, status=status)
        call check(status == CHFL_SUCCESS, "cell%set_type")

        call cell%set_angles([80d0, 89d0, 100d0], status=status)
        call check(status == CHFL_SUCCESS, "cell%set_angles")
        call cell%angles(angles, status=status)
        call check(status == CHFL_SUCCESS, "cell%angles")
        call check(all(angles == [80.0, 89.0, 100.0]), "cell%angles")

        call cell%free(status=status)
        call check(status == CHFL_SUCCESS, "cell%free")
    end subroutine

    subroutine test_matrix()
        implicit none
        type(chfl_cell) :: cell
        real(real64), dimension(3, 3) :: expected, matrix
        integer :: status, i, j

        call cell%init([10d0, 20d0, 30d0], status=status)
        call check(status == CHFL_SUCCESS, "cell%init")

        expected = reshape([10, 0, 0, &
                            0, 20, 0, &
                            0, 0, 30], [3, 3])
        call cell%matrix(matrix, status=status)
        call check(status == CHFL_SUCCESS, "cell%matrix")
        do i=1,3
            do j=1,3
                call check(matrix(i, j) - expected(i, j) < 1d-10, "cell%matrix")
            end do
        end do

        call cell%free(status=status)
        call check(status == CHFL_SUCCESS, "cell%free")
    end subroutine
end program
