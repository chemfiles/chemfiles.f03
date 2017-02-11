program atom_test
    use iso_fortran_env, only: real64, int64
    use chemfiles
    use testing
    implicit none

    call test_name()
    call test_type()
    call test_mass()
    call test_charge()
    call test_radius()
    call test_atomic_number()

contains
    subroutine test_name()
        implicit none
        type(chfl_atom) :: atom
        character(len=32) :: name
        integer :: status

        call atom%init("He", status=status)
        call check(status == CHFL_SUCCESS, "atom%init")

        call atom%name(name, len(name, int64), status=status)
        call check(status == CHFL_SUCCESS, "atom%name")
        call check(name == "He", "atom%name")

        call atom%full_name(name, len(name, int64), status=status)
        call check(status == CHFL_SUCCESS, "atom%full_name")
        call check(name == "Helium", "atom%full_name")

        call atom%set_name("Zn", status=status)
        call check(status == CHFL_SUCCESS, "atom%set_name")

        call atom%name(name, len(name, int64), status=status)
        call check(status == CHFL_SUCCESS, "atom%name")
        call check(name == "Zn", "atom%name")

        call atom%free(status=status)
        call check(status == CHFL_SUCCESS, "atom%free")
    end subroutine

    subroutine test_type()
        implicit none
        type(chfl_atom) :: atom
        character(len=32) :: name, atom_type
        integer :: status

        call atom%init("He", status=status)
        call check(status == CHFL_SUCCESS, "atom%init")

        call atom%type(atom_type, len(atom_type, int64), status=status)
        call check(status == CHFL_SUCCESS, "atom%type")
        call check(atom_type == "He", "atom%type")

        call atom%set_type("Zn", status=status)
        call check(status == CHFL_SUCCESS, "atom%set_type")

        call atom%type(atom_type, len(atom_type, int64), status=status)
        call check(status == CHFL_SUCCESS, "atom%type")
        call check(atom_type == "Zn", "atom%type")

        call atom%full_name(name, len(name, int64), status=status)
        call check(status == CHFL_SUCCESS, "atom%full_name")
        call check(name == "Zinc", "atom%full_name")

        call atom%free(status=status)
        call check(status == CHFL_SUCCESS, "atom%free")
    end subroutine

    subroutine test_mass()
        implicit none
        type(chfl_atom) :: atom
        real(real64) :: mass = 0
        integer :: status

        call atom%init("He", status=status)
        call check(status == CHFL_SUCCESS, "atom%init")

        call atom%mass(mass, status=status)
        call check(status == CHFL_SUCCESS, "atom%mass")
        call check(abs(mass - 4.002602) < 1d-6, "atom%mass")

        call atom%set_mass(678.0d0, status=status)
        call check(status == CHFL_SUCCESS, "atom%set_mass")

        call atom%mass(mass, status=status)
        call check(status == CHFL_SUCCESS, "atom%mass")
        call check(mass == 678.0, "atom%mass")

        call atom%free(status=status)
        call check(status == CHFL_SUCCESS, "atom%free")
    end subroutine

    subroutine test_charge()
        implicit none
        type(chfl_atom) :: atom
        real(real64) :: charge = 0
        integer :: status

        call atom%init("He", status=status)
        call check(status == CHFL_SUCCESS, "atom%init")

        call atom%charge(charge, status=status)
        call check(status == CHFL_SUCCESS, "atom%charge")
        call check(charge == 0.0, "atom%charge")

        call atom%set_charge(-1.5d0, status=status)
        call check(status == CHFL_SUCCESS, "atom%set_charge")

        call atom%charge(charge, status=status)
        call check(status == CHFL_SUCCESS, "atom%charge")
        call check(charge == -1.5, "atom%charge")

        call atom%free(status=status)
        call check(status == CHFL_SUCCESS, "atom%free")
    end subroutine

    subroutine test_radius()
        implicit none
        type(chfl_atom) :: atom
        real(real64) :: radius = 0.0
        integer :: status

        call atom%init("Zn", status=status)
        call check(status == CHFL_SUCCESS, "atom%init")

        call atom%vdw_radius(radius, status=status)
        call check(status == CHFL_SUCCESS, "atom%vdw_radius")
        call check(abs(radius - 2.1) < 1d-6, "atom%vdw_radius")

        call atom%covalent_radius(radius, status=status)
        call check(status == CHFL_SUCCESS, "atom%covalent_radius")
        call check(abs(radius - 1.31) < 1d-6, "atom%covalent_radius")

        call atom%free(status=status)
        call check(status == CHFL_SUCCESS, "atom%free")
    end subroutine

    subroutine test_atomic_number()
        implicit none
        type(chfl_atom) :: atom
        integer(int64) :: number = 0
        integer :: status

        call atom%init("Zn", status=status)
        call check(status == CHFL_SUCCESS, "atom%init")

        call atom%atomic_number(number, status=status)
        call check(status == CHFL_SUCCESS, "atom%atomic_number")
        call check(number == 30, "atom%atomic_number")

        call atom%free(status=status)
        call check(status == CHFL_SUCCESS, "atom%free")
    end subroutine
end program
