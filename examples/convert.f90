!* File convert.f90, example for the chemfiles library
!* Any copyright is dedicated to the Public Domain.
!* http://creativecommons.org/publicdomain/zero/1.0/
program convert
    use iso_fortran_env, only: int64
    use chemfiles
    implicit none

    type(chfl_trajectory) :: input, ouput_file
    type(chfl_frame) :: frame
    type(chfl_cell) :: cell
    type(chfl_topology) :: water_topology
    type(chfl_atom) :: O, H

    integer(int64) :: nsteps, i, status

    call input%open("water.xyz", "r", status=status)
    if (status /= 0) stop "Error while opening input file"
    call ouput_file%open("water.pdb", "w")

    call frame%init(0)
    call water_topology%init()
    ! Orthorombic UnitCell with lengths of 20, 15 and 35 A
    call cell%init(20d0, 15d0, 35d0)

    ! Create Atoms
    call O%init("O")
    call H%init("H")

    ! Fill the topology with one water molecule
    call water_topology%append(O)
    call water_topology%append(H)
    call water_topology%append(H)
    call water_topology%add_bond(0, 1)
    call water_topology%add_bond(0, 2)

    call input%nsteps(nsteps)

    do i=1,nsteps
        call input%read(frame, status)
        if (status /= 0) stop "Error while reading input file"
        ! Set the frame cell and topology
        call frame%set_cell(cell)
        call frame%set_topology(water_topology)
        ! Write the frame to the output file, using PDB format
        call ouput_file%write(frame, status)
        if (status /= 0) stop "Error while writing ouput file"
    end do

end program
