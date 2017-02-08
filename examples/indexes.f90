!* File indexes.f90, example for the chemfiles library
!* Any copyright is dedicated to the Public Domain.
!* http://creativecommons.org/publicdomain/zero/1.0/
program indexes_
    use iso_fortran_env, only: real64, int64
    use chemfiles
    implicit none
    ! Chemfiles types declaration uses the "chfl_" prefix
    type(chfl_trajectory) :: trajectory
    type(chfl_frame) :: frame

    real(real64), dimension(:, :), pointer :: positions
    integer(int64), dimension(:), allocatable :: indexes
    integer(int64) :: natoms, i, j
    integer :: status

    call frame%init()
    call trajectory%open("filename.xyz", "r")

    call trajectory%read(frame, status=status)
    if (status /= 0) then
        stop "Error"
    end if

    call frame%positions(positions, natoms)
    allocate(indexes(natoms))

    indexes = -1
    j = 1
    do i=1,natoms
        if (positions(1, i) < 5) then
            indexes(j) = i
            j = j + 1
        end if
    end do

    write(*,*) "Atoms with x < 5: "
    do i=1,natoms
        if (indexes(i) == -1) then
            exit
        end if
        write(*,*) "  - ", indexes(i)
    end do

    ! Cleanup the allocated memory
    deallocate(indexes, positions)
    call trajectory%close()
    call frame%free()
end program
