program atom_test
    use iso_fortran_env, only: real64, int64
    use chemfiles
    use testing

    implicit none
    type(chfl_atom) :: atom
    real(real64) :: radius = 0.0, mass = 0.0, charge = 0.0
    integer :: status
    integer(int64) :: number
    character(len=32) :: name

    call atom%init("He", status=status)
    call check(status == 0, "atom%init")

    call atom%mass(mass, status=status)
    call check(status == 0, "atom%mass")
    call check(abs(mass - 4.002602) < 1d-6, "atom%mass")

    call atom%charge(charge, status=status)
    call check(status == 0, "atom%charge")
    call check(charge == 0.0, "atom%charge")

    call atom%name(name, len(name, int64), status=status)
    call check(status == 0, "atom%name")
    call check(name == "He", "atom%name")

    call atom%full_name(name, len(name, int64), status=status)
    call check(status == 0, "atom%full_name")
    call check(name == "Helium", "atom%full_name")

    call atom%set_mass(678.0d0, status=status)
    call check(status == 0, "atom%set_mass")
    call atom%mass(mass, status=status)
    call check(status == 0, "atom%mass")
    call check(mass == 678.0, "atom%mass")

    call atom%set_charge(-1.5d0, status=status)
    call check(status == 0, "atom%set_charge")
    call atom%charge(charge, status=status)
    call check(status == 0, "atom%charge")
    call check(charge == -1.5, "atom%charge")

    call atom%set_name("Zn", status=status)
    call check(status == 0, "atom%set_name")
    call atom%name(name, len(name, int64), status=status)
    call check(status == 0, "atom%name")
    call check(name == "Zn", "atom%name")

    call atom%full_name(name, len(name, int64), status=status)
    call check(status == 0, "atom%full_name")
    call check(name == "Helium", "atom%full_name")

    call atom%type(name, len(name, int64), status=status)
    call check(status == 0, "atom%type")
    call check(name == "He", "atom%type")

    call atom%set_type("Zn", status=status)
    call check(status == 0, "atom%set_type")
    call atom%type(name, len(name, int64), status=status)
    call check(status == 0, "atom%type")
    call check(name == "Zn", "atom%type")

    call atom%full_name(name, len(name, int64), status=status)
    call check(status == 0, "atom%full_name")
    call check(name == "Zinc", "atom%full_name")

    call atom%vdw_radius(radius, status=status)
    call check(status == 0, "atom%vdw_radius")
    call check(abs(radius - 2.1) < 1d-6, "atom%vdw_radius")

    call atom%covalent_radius(radius, status=status)
    call check(status == 0, "atom%covalent_radius")
    call check(abs(radius - 1.31) < 1d-6, "atom%covalent_radius")

    call atom%atomic_number(number, status=status)
    call check(status == 0, "atom%atomic_number")
    call check(number == 30, "atom%atomic_number")

    call atom%free(status=status)
    call check(status == 0, "atom%free")
end program
