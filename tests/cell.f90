program cell_test
    use iso_fortran_env, only: real64
    use chemfiles
    use testing

    implicit none
    type(chfl_cell) :: cell
    real(real64) :: a, b, c, V
    real(real64), dimension(3, 3) :: expected_mat, mat
    integer :: status, i, j
    integer(CHFL_CELL_TYPES) :: cell_type

    call cell%triclinic(12d0, 30d0, 24d0, 90d0, 90d0, 120d0, status=status)
    call check((status == 0), "cell%triclinic")
    call cell%lengths(a, b, c, status=status)
    call check((status == 0), "cell%lengths")
    call check((a == 12.0), "cell%lengths")
    call check((b == 30.0), "cell%lengths")
    call check((c == 24.0), "cell%lengths")

    call cell%angles(a, b, c, status=status)
    call check((status == 0), "cell%angles")
    call check((a == 90.0), "cell%angles")
    call check((b == 90.0), "cell%angles")
    call check((c == 120.0), "cell%angles")

    call cell%free(status=status)
    call check((status == 0), "cell%free")

    call cell%init(2d0, 3d0, 4d0, status=status)
    call check((status == 0), "cell%init")

    call cell%lengths(a, b, c, status=status)
    call check((status == 0), "cell%lengths")
    call check((a == 2.0), "cell%lengths")
    call check((b == 3.0), "cell%lengths")
    call check((c == 4.0), "cell%lengths")

    call cell%angles(a, b, c, status=status)
    call check((status == 0), "cell%angles")
    call check((a == 90.0), "cell%angles")
    call check((b == 90.0), "cell%angles")
    call check((c == 90.0), "cell%angles")

    call cell%volume(V, status=status)
    call check((status == 0), "cell%volume")
    call check((V == 2.0*3.0*4.0), "cell%volume")

    call cell%set_lengths(10d0, 20d0, 30d0, status=status)
    call check((status == 0), "cell%set_lengths")
    call cell%lengths(a, b, c, status=status)
    call check((status == 0), "cell%lengths")
    call check((a == 10.0), "cell%lengths")
    call check((b == 20.0), "cell%lengths")
    call check((c == 30.0), "cell%lengths")

    call chfl_log_silent(status=status)
    call check((status == 0), "chfl_log_silent")
    call cell%set_angles(80d0, 89d0, 100d0, status=status)
    call check((status /= 0), "cell%set_angles")
    call chfl_log_stderr(status=status)
    call check((status == 0), "chfl_log_stderr")

    expected_mat = reshape([10, 0, 0, &
                            0, 20, 0, &
                            0, 0, 30], [3, 3])
    call cell%matrix(mat, status=status)
    call check((status == 0), "cell%matrix")
    do i=1,3
        do j=1,3
            call check((mat(i, j) - expected_mat(i, j) < 1d-10), "cell%matrix")
        end do
    end do

    call cell%type(cell_type, status=status)
    call check((status == 0), "cell%type")
    call check((cell_type == CHFL_CELL_ORTHORHOMBIC), "cell%type")

    call cell%set_type(CHFL_CELL_TRICLINIC, status=status)
    call check((status == 0), "cell%set_type")
    call cell%type(cell_type, status=status)
    call check((status == 0), "cell%type")
    call check((cell_type == CHFL_CELL_TRICLINIC), "cell%type")

    call cell%set_angles(80d0, 89d0, 100d0, status=status)
    call check((status == 0), "cell%set_angles")
    call cell%angles(a, b, c, status=status)
    call check((status == 0), "cell%angles")
    call check((a == 80.0), "cell%angles")
    call check((b == 89.0), "cell%angles")
    call check((c == 100.0), "cell%angles")

    call cell%free(status=status)
    call check((status == 0), "cell%free")
end program
