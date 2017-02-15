program residue_test
    use iso_fortran_env, only: int64
    use chemfiles
    use testing
    implicit none

    call test_copy()
    call test_name()
    call test_id()
    call test_size()
    call test_contains()

contains
    subroutine test_copy()
        implicit none
        type(chfl_residue) :: residue, cloned
        integer :: status

        call residue%init("Res", status=status)
        call check(status == 0, "residue%init")

        call residue%add_atom(int(0, int64), status=status)
        call check(status == 0, "residue%add_atom")

        call cloned%copy(residue, status=status)
        call check(status == 0, "residue%copy")


        ! TODO: test size change


        call residue%free(status=status)
        call check(status == 0, "residue%free")
        call cloned%free(status=status)
        call check(status == 0, "residue%free")
    end subroutine

    subroutine test_name()
        implicit none
        type(chfl_residue) :: residue
        character(len=32) :: name
        integer :: status

        call residue%init("Foo", status=status)
        call check(status == CHFL_SUCCESS, "residue%init")

        call residue%name(name, len(name, int64), status=status)
        call check(status == CHFL_SUCCESS, "residue%name")
        call check(name == "Foo", "residue%name")

        call residue%free(status=status)
        call check(status == CHFL_SUCCESS, "residue%free")
    end subroutine

    subroutine test_id()
        implicit none
        type(chfl_residue) :: residue
        integer(int64) :: id
        integer :: status

        call residue%init("Foo", int(56, int64), status=status)
        call check(status == CHFL_SUCCESS, "residue%init")

        call residue%id(id, status=status)
        call check(status == CHFL_SUCCESS, "residue%id")
        call check(id == 56, "residue%id")

        call residue%free(status=status)
        call check(status == CHFL_SUCCESS, "residue%free")

        call residue%init("Bar", status=status)
        call check(status == CHFL_SUCCESS, "residue%init")

        call residue%id(id, status=status)
        call check(status == CHFL_SUCCESS, "residue%id")
        call check(id == -1, "residue%id")

        call residue%free(status=status)
        call check(status == CHFL_SUCCESS, "residue%free")
    end subroutine

    subroutine test_size()
        implicit none
        type(chfl_residue) :: residue
        integer(int64) :: natoms
        integer :: status

        call residue%init("Foo", status=status)
        call check(status == CHFL_SUCCESS, "residue%init")

        call residue%atoms_count(natoms, status=status)
        call check(status == CHFL_SUCCESS, "residue%natoms")
        call check(natoms == 0, "residue%natoms")

        call residue%add_atom(int(0, int64), status=status)
        call check(status == 0, "residue%add_atom")
        call residue%add_atom(int(1, int64), status=status)
        call check(status == 0, "residue%add_atom")
        call residue%add_atom(int(2, int64), status=status)
        call check(status == 0, "residue%add_atom")

        call residue%atoms_count(natoms, status=status)
        call check(status == CHFL_SUCCESS, "residue%natoms")
        call check(natoms == 3, "residue%natoms")

        call residue%free(status=status)
        call check(status == CHFL_SUCCESS, "residue%free")
    end subroutine

    subroutine test_contains()
        implicit none
        type(chfl_residue) :: residue
        logical(1) :: contains
        integer :: status

        call residue%init("Foo", status=status)
        call check(status == CHFL_SUCCESS, "residue%init")

        call residue%add_atom(int(0, int64), status=status)
        call check(status == 0, "residue%add_atom")
        call residue%add_atom(int(1, int64), status=status)
        call check(status == 0, "residue%add_atom")
        call residue%add_atom(int(2, int64), status=status)
        call check(status == 0, "residue%add_atom")

        call residue%contains(int(2, int64), contains, status=status)
        call check(status == CHFL_SUCCESS, "residue%contains")
        call check(contains .eqv. .true., "residue%contains")

        call residue%contains(int(20, int64), contains, status=status)
        call check(status == CHFL_SUCCESS, "residue%contains")
        call check(contains .eqv. .false., "residue%contains")

        call residue%free(status=status)
        call check(status == CHFL_SUCCESS, "residue%free")
    end subroutine
end program
