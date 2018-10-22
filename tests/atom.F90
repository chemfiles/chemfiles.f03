#include "check.inc"

program atom_test
    use iso_fortran_env, only: real64, int64
    use chemfiles
    implicit none

    call test_copy()
    call test_name()
    call test_type()
    call test_mass()
    call test_charge()
    call test_radius()
    call test_atomic_number()
    ! call test_properties()

contains
    subroutine test_copy()
        implicit none
        type(chfl_atom) :: atom, cloned
        character(len=32) :: name
        integer :: status

        call atom%init('He', status=status)
        CHECK(status == 0)
        call cloned%copy(atom, status=status)
        CHECK(status == 0)

        call atom%name(name, len(name, int64), status=status)
        CHECK(status == 0)
        CHECK(name == 'He')
        name = ''
        call cloned%name(name, len(name, int64), status=status)
        CHECK(status == 0)
        CHECK(name == 'He')

        call atom%set_name('Zn', status=status)
        CHECK(status == 0)

        call atom%name(name, len(name, int64), status=status)
        CHECK(status == 0)
        CHECK(name == 'Zn')
        name = ''
        call cloned%name(name, len(name, int64), status=status)
        CHECK(status == 0)
        CHECK(name == 'He')

        call atom%free(status=status)
        CHECK(status == 0)
        call cloned%free(status=status)
        CHECK(status == 0)
    end subroutine

    subroutine test_name()
        implicit none
        type(chfl_atom) :: atom
        character(len=32) :: name
        integer :: status

        call atom%init('He', status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%name(name, len(name, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(name == 'He')

        call atom%full_name(name, len(name, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(name == 'Helium')

        call atom%set_name('Zn', status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%name(name, len(name, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(name == 'Zn')

        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_type()
        implicit none
        type(chfl_atom) :: atom
        character(len=32) :: name, atom_type
        integer :: status

        call atom%init('He', status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%type(atom_type, len(atom_type, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(atom_type == 'He')

        call atom%set_type('Zn', status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%type(atom_type, len(atom_type, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(atom_type == 'Zn')

        call atom%full_name(name, len(name, int64), status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(name == 'Zinc')

        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_mass()
        implicit none
        type(chfl_atom) :: atom
        real(real64) :: mass = 0
        integer :: status

        call atom%init('He', status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%mass(mass, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(abs(mass - 4.002602) < 1d-6)

        call atom%set_mass(678.0d0, status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%mass(mass, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(mass == 678.0)

        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_charge()
        implicit none
        type(chfl_atom) :: atom
        real(real64) :: charge = 0
        integer :: status

        call atom%init('He', status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%charge(charge, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(charge == 0.0)

        call atom%set_charge(-1.5d0, status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%charge(charge, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(charge == -1.5)

        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_radius()
        implicit none
        type(chfl_atom) :: atom
        real(real64) :: radius = 0.0
        integer :: status

        call atom%init('Zn', status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%vdw_radius(radius, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(abs(radius - 2.1) < 1d-6)

        call atom%covalent_radius(radius, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(abs(radius - 1.31) < 1d-6)

        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    subroutine test_atomic_number()
        implicit none
        type(chfl_atom) :: atom
        integer(int64) :: number = 0
        integer :: status

        call atom%init('Zn', status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%atomic_number(number, status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(number == 30)

        call atom%free(status=status)
        CHECK(status == CHFL_SUCCESS)
    end subroutine

    ! subroutine test_properties()
    !     implicit none
    !     type(chfl_atom) :: atom
    !     type(chfl_property) :: property
    !     real(real64) :: value
    !     integer :: status
    !
    !     call atom%init('He', status=status)
    !     CHECK(status == CHFL_SUCCESS)
    !
    !     call property%double(42d0, status=status)
    !     CHECK(status == CHFL_SUCCESS)
    !
    !     call atom%set_property("foo", property, status=status)
    !     CHECK(status == CHFL_SUCCESS)
    !
    !     call property%free(status=status)
    !     CHECK(status == CHFL_SUCCESS)
    !
    !     call property%from_atom(atom, "foo", status=status)
    !     CHECK(status == CHFL_SUCCESS)
    !
    !     call property%get_double(value, status=status)
    !     CHECK(status == CHFL_SUCCESS)
    !     CHECK(value == 42d0)
    !
    !     call property%free(status=status)
    !     CHECK(status == CHFL_SUCCESS)
    !     call atom%free(status=status)
    !     CHECK(status == CHFL_SUCCESS)
    ! end subroutine
end program
