! This file is an example for the chemfiles library
! Any copyright is dedicated to the Public Domain.
! http://creativecommons.org/publicdomain/zero/1.0/
program indexes
    use iso_fortran_env, only: int64, real64
    use chemfiles
    implicit none

    type(chfl_trajectory) :: file
    type(chfl_frame) :: frame
    real(real64), dimension(:, :), pointer :: positions
    integer(int64), dimension(:), allocatable :: less_than_five
    integer(int64) :: natoms = 0
    integer :: i, matched

    call file%open("filename.xyz", 'r')
    call frame%init()
    call file%read(frame)

    call frame%positions(positions, natoms)
    allocate(less_than_five(natoms))

    matched = 0
    do i = 1,natoms
        if (positions(0, i) .lt. 5) then
            less_than_five(matched) = i
            matched = matched + 1
        end if
    end do

    print*, "Atoms with x < 5:"
    do i=1,matched
        print*, "  - ", less_than_five(i)
    end do

    deallocate(less_than_five)
    call frame%free()
    call file%close()
end program
