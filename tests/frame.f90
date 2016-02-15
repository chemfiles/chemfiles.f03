program frame_test
    use iso_fortran_env, only: real32, real64, int64
    use chemfiles
    use testing

    implicit none
    type(chfl_frame) :: frame
    type(chfl_cell) :: cell
    type(chfl_topology) :: topology
    type(chfl_atom) :: Zn, Ar, atom
    integer :: status, i, j
    integer(kind=int64) :: natoms, step
    real(kind=real32), dimension(:, :), pointer :: set_data, check_data
    real(kind=real64) :: a, b, c
    character(len=32) :: name
    logical(1) :: has_vel = .true.

    call frame%init(5, status=status)
    call check((status == 0), "frame%init")

    call frame%atoms_count(natoms, status=status)
    call check((status == 0), "frame%atoms_count")
    call check((natoms == 5), "frame%atoms_count")

    call frame%step(step, status=status)
    call check((status == 0), "frame%step")
    call check((step == 0), "frame%step")

    call frame%set_step(42, status=status)
    call check((status == 0), "frame%set_step")
    call frame%step(step, status=status)
    call check((status == 0), "frame%step")
    call check((step == 42), "frame%set_step")

    call frame%resize(4, status=status)
    call check((status == 0), "frame%resize")

    call frame%positions(set_data, natoms, status=status)
    call check((status == 0), "frame%positions")
    call check((natoms == 4), "frame%positions")
    do i=1,3
        do j=1,4
            set_data(i, j) = real(i * j)
        end do
    end do

    call frame%positions(check_data, natoms, status=status)
    call check((status == 0), "frame%positions")
    do i=1,3
        do j=1,natoms
            call check((abs(check_data(i, j) - real(i * j)) < 1e-9), "frame%positions")
        end do
    end do

    call frame%has_velocities(has_vel, status=status)
    call check((status == 0), "frame%has_velocities")
    call check((has_vel .eqv. .false.), "frame%has_velocities")

    call frame%add_velocities(status=status)
    call check((status == 0), "frame%add_velocities")

    call frame%has_velocities(has_vel, status=status)
    call check((status == 0), "frame%has_velocities")
    call check((has_vel .eqv. .true.), "frame%has_velocities")

    call frame%velocities(set_data, natoms, status=status)
    call check((status == 0), "frame%velocities")
    call check((natoms == 4), "frame%velocities")
    do i=1,3
        do j=1,4
            set_data(i, j) = real(i * j)
        end do
    end do

    call frame%velocities(check_data, natoms, status=status)
    call check((status == 0), "frame%velocities")
    do i=1,3
        do j=1,natoms
            call check((abs(check_data(i, j) - real(i * j)) < 1e-9), "frame%velocities")
        end do
    end do

    call cell%init(3d0, 4d0, 5d0)
    call frame%set_cell(cell, status=status)
    call check((status == 0), "frame%set_cell")
    call cell%free()

    call cell%from_frame(frame)
    call cell%lengths(a, b, c)
    call check((a == 3.0), "frame%cell")
    call check((b == 4.0), "frame%cell")
    call check((c == 5.0), "frame%cell")
    call cell%free()

    call topology%init()
    call Zn%init("Zn")
    call Ar%init("Ar")
    call topology%append(Zn)
    call topology%append(Ar)

    call frame%set_topology(topology, status=status)
    call check((status == 0), "frame%set_topology")

    call topology%free()
    call Zn%free()
    call Ar%free()

    call topology%from_frame(frame, status=status)
    call check((status == 0), "topology%from_frame")

    call atom%from_topology(topology, 0, status=status)
    call check((status == 0), "atom%from_topology")
    call atom%name(name, len(name), status=status)
    call check((status == 0), "atom%name")
    call check((name == "Zn"), "atom%name")
    call atom%free()

    call atom%from_frame(frame, 1, status=status)
    call check((status == 0), "atom%from_frame")
    call atom%name(name, len(name), status=status)
    call check((status == 0), "atom%name")
    call check((name == "Ar"), "atom%name")

    call frame%free(status=status)
    call check((status == 0), "frame%free")
end program