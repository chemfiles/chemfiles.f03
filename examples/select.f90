! This file is an example for the chemfiles library
! Any copyright is dedicated to the Public Domain.
! http://creativecommons.org/publicdomain/zero/1.0/
program select
    use iso_fortran_env, only: int64
    use chemfiles
    implicit none

    type(chfl_trajectory) :: input, output
    type(chfl_frame) :: frame
    type(chfl_selection) :: selection
    type(chfl_match), allocatable, dimension(:) :: matches
    integer(int64), allocatable, dimension(:) :: to_remove
    integer(int64) :: matching, i, nsteps, step
    integer :: status

    call input%open("input.arc", 'r')
    call output%open("output.pdb", 'w')
    call frame%init()
    call selection%init("name Zn or name N")

    nsteps = 0
    call input%nsteps(nsteps)

    do step=1,nsteps
        call input%read(frame)

        matching = 0
        call selection%evaluate(frame, matching)
        allocate(matches(matching))
        call selection%matches(matches, matching)

        allocate(to_remove(matching))
        do i=1,matching
            to_remove(i) = matches(i)%atoms(1)
        end do

        call sort(to_remove)
        do i = matching - 1, 0, -1
            call frame%remove(to_remove(i))
        end do

        call output%write(frame)
        deallocate(matches, to_remove)
    end do

    call selection%free()
    call frame%free()
    call input%close()
    call output%close()

contains
    ! A very simple and ineficient sorting routine
    subroutine sort(array)
        integer(int64), intent(inout) :: array(:)
        integer(int64) :: i, j, min, pos, tmp
        do i = 1, size(array) - 1
            min = array(i)
            pos = i
            do j = i, size(array)
                if (array(j) < min) then
                    min = array(j)
                    pos = j
                end if
            end do
            tmp = array(i)
            array(i) = min
            array(pos) = tmp
        end do
    end subroutine
end program
