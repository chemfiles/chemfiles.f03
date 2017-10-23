!* File rmsd.f90, example for the chemfiles library
!* Any copyright is dedicated to the Public Domain.
!* http://creativecommons.org/publicdomain/zero/1.0/
program rmsd_
    use iso_fortran_env, only: real64, int64
    use chemfiles
    implicit none

    type(chfl_trajectory) :: trajectory
    type(chfl_frame)      :: frame
    real(real64), dimension(:, :), pointer :: positions
    real(real64), dimension(:),    allocatable :: distances
    integer(int64) :: nsteps = 0, natoms=0, i
    real(real64) :: distance = 0, mean = 0, rmsd = 0
    integer :: status

    call trajectory%open("filename.nc", "r", status=status)
    if (status /= 0) stop "Error while opening input file"
    call frame%init()

    call trajectory%nsteps(nsteps)
    allocate(distances(nsteps))

    ! Accumulate the distances to the origin of the 10th atom throughtout the
    ! trajectory
    do i=1,nsteps
        call trajectory%read(frame, status)
        if (status /= 0) stop "Error while reading the frame"

        ! Only allocate on the first iteration. This assume a constant number
        ! of particles
        if (i == 1) then
            call frame%atoms_count(natoms)
            allocate(positions(3, natoms))
        end if

        ! Position of the 10th atom
        call frame%positions(positions, natoms, status)
        if (status /= 0) stop "Error while getting the positions"
        distance = sqrt(positions(1, 10)*positions(1, 10) + &
                        positions(2, 10)*positions(2, 10) + &
                        positions(3, 10)*positions(3, 10))
        distances(i) = distance
    end do

    do i=1,nsteps
        mean = mean + distances(i)
    end do
    mean = mean / nsteps

    do i=1,nsteps
        rmsd = rmsd + (mean - distances(i))*(mean - distances(i));
    end do
    rmsd = rmsd / nsteps;
    rmsd = sqrt(rmsd);

    write(*, *) "Root-mean square displacement is: ", rmsd

    ! Cleanup the allocated memory
    call trajectory%close()
    call frame%free()
    deallocate(distances, positions)
end program
