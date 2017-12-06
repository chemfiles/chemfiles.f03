! This file is an example for the chemfiles library
! Any copyright is dedicated to the Public Domain.
! http://creativecommons.org/publicdomain/zero/1.0/
program generate
    use iso_fortran_env, only: int64, real64
    use chemfiles
    implicit none

    type(chfl_topology) :: topology
    type(chfl_atom) :: O, H, C
    type(chfl_frame) :: frame
    type(chfl_cell) :: cell
    type(chfl_trajectory) :: trajectory
    real(real64), dimension(:, :), pointer :: positions
    integer(int64) :: natoms

    call topology%init()
    call H%init("H")
    call O%init("H")
    call C%init("H")

    call topology%add_atom(H)
    call topology%add_atom(O)
    call topology%add_atom(H)

    call topology%add_bond(int(0, int64), int(1, int64))
    call topology%add_bond(int(2, int64), int(1, int64))

    call frame%init()
    call frame%resize(int(3, int64))
    call frame%set_topology(topology)
    call topology%free()

    call frame%positions(positions, natoms)
    positions(:, 1) = [1d0, 0d0, 0d0]
    positions(:, 2) = [0d0, 0d0, 0d0]
    positions(:, 3) = [0d0, 1d0, 0d0]

    call frame%add_atom(O, [5d0, 0d0, 0d0])
    call frame%add_atom(C, [6d0, 0d0, 0d0])
    call frame%add_atom(O, [7d0, 0d0, 0d0])
    call frame%add_bond(int(3, int64), int(4, int64))
    call frame%add_bond(int(4, int64), int(5, int64))

    call cell%init([10d0, 10d0, 10d0])
    call frame%set_cell(cell)
    call cell%free()

    call trajectory%open("water-co2.pdb", 'w')
    call trajectory%write(frame)
    call trajectory%close()

    call frame%free()
    call C%free()
    call H%free()
    call O%free()
end program
