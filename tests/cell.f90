program cell_test
    use iso_fortran_env, only: real64
    use chemfiles
    use testing

    implicit none
    type(chfl_cell) :: cell
    real(real64) :: volume
    real(real64), dimension(3, 3) :: expected, matrix
    real(real64), dimension(3) :: lengths, angles
    integer :: status, i, j
    integer(chfl_cell_shape_t) :: shape

    call cell%triclinic([12d0, 30d0, 24d0], [90d0, 90d0, 120d0], status=status)
    call check(status == 0, "cell%triclinic")
    call cell%lengths(lengths, status=status)
    call check(status == 0, "cell%lengths")
    call check(lengths(1) == 12.0, "cell%lengths")
    call check(lengths(2) == 30.0, "cell%lengths")
    call check(lengths(3) == 24.0, "cell%lengths")

    call cell%angles(angles, status=status)
    call check(status == 0, "cell%angles")
    call check(angles(1) == 90.0, "cell%angles")
    call check(angles(2) == 90.0, "cell%angles")
    call check(angles(3) == 120.0, "cell%angles")

    call cell%free(status=status)
    call check(status == 0, "cell%free")

    call cell%init([2d0, 3d0, 4d0], status=status)
    call check(status == 0, "cell%init")

    call cell%lengths(lengths, status=status)
    call check(status == 0, "cell%lengths")
    call check(lengths(1) == 2.0, "cell%lengths")
    call check(lengths(2) == 3.0, "cell%lengths")
    call check(lengths(3) == 4.0, "cell%lengths")

    call cell%angles(angles, status=status)
    call check(status == 0, "cell%angles")
    call check(angles(1) == 90.0, "cell%angles")
    call check(angles(2) == 90.0, "cell%angles")
    call check(angles(3) == 90.0, "cell%angles")

    call cell%volume(volume, status=status)
    call check(status == 0, "cell%volume")
    call check(volume == 2.0*3.0*4.0, "cell%volume")

    call cell%set_lengths([10d0, 20d0, 30d0], status=status)
    call check(status == 0, "cell%set_lengths")
    call cell%lengths(lengths, status=status)
    call check(status == 0, "cell%lengths")
    call check(lengths(1) == 10.0, "cell%lengths")
    call check(lengths(2) == 20.0, "cell%lengths")
    call check(lengths(3) == 30.0, "cell%lengths")

    call cell%set_angles([80d0, 89d0, 100d0], status=status)
    call check(status /= 0, "cell%set_angles")

    expected = reshape([10, 0, 0, &
                        0, 20, 0, &
                        0, 0, 30], [3, 3])
    call cell%matrix(matrix, status=status)
    call check(status == 0, "cell%matrix")
    do i=1,3
        do j=1,3
            call check(matrix(i, j) - expected(i, j) < 1d-10, "cell%matrix")
        end do
    end do

    call cell%shape(shape, status=status)
    call check(status == 0, "cell%type")
    call check(shape == CHFL_CELL_ORTHORHOMBIC, "cell%type")

    call cell%set_shape(CHFL_CELL_TRICLINIC, status=status)
    call check(status == 0, "cell%set_type")
    call cell%shape(shape, status=status)
    call check(status == 0, "cell%type")
    call check(shape == CHFL_CELL_TRICLINIC, "cell%type")

    call cell%set_angles([80d0, 89d0, 100d0], status=status)
    call check(status == 0, "cell%set_angles")
    call cell%angles(angles, status=status)
    call check(status == 0, "cell%angles")
    call check(angles(1) == 80.0, "cell%angles")
    call check(angles(2) == 89.0, "cell%angles")
    call check(angles(3) == 100.0, "cell%angles")

    call cell%free(status=status)
    call check(status == 0, "cell%free")
end program
