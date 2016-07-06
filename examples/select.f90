!* File select.f90, example for the chemfiles library
!* Any copyright is dedicated to the Public Domain.
!* http://creativecommons.org/publicdomain/zero/1.0/
program select
    use iso_fortran_env, only: int64
    use chemfiles
    implicit none

    type(chfl_trajectory) :: file
    type(chfl_frame) :: frame
    type(chfl_selection) :: selection
    type(chfl_match), allocatable, dimension(:) :: matches
    integer(int64) :: matching, i
    integer :: status

    call file%open("filename.xyz", "r")
    call frame%init(int(0, int64))
    call file%read(frame, status=status)
    if (status /= 0) then
        ! handle error
    endif

    ! Create a selection for all atoms with "Zn" name
    call selection%init("name Zn")
    matching = 0

    ! Get the number of matching atoms from the frame
    call selection%evalutate(frame, matching)
    print *, "We have ", matching, " zinc in the frame"
    allocate(matches(matching))

    ! Get the matching atoms
    call selection%matches(matches, matching)
    do i=1, matching
        print *, matches(i)%atoms(1), "is a zinc"
    enddo

    call selection%free()
    deallocate(matches)

    ! Create a selection for multiple atoms
    call selection%init("angles: name($1) H and name($2) O and name($3) H")
    call selection%evalutate(frame, matching)
    print *, "We have ", matching, " water in the frame"
    allocate(matches(matching))

    ! Get the matching atoms
    call selection%matches(matches, matching)
    do i=1, matching
        print *, i, "is a zinc"
    enddo

    ! Get the matching atoms
    call selection%matches(matches, matching)
    do i=1, matching
        print *, matches(i)%atoms(1), "-", &
                 matches(i)%atoms(2), "-", &
                 matches(i)%atoms(3),  " is a water molecule"
    enddo

    call selection%free()
    call file%close()
    call frame%free()
    deallocate(matches)
end program
