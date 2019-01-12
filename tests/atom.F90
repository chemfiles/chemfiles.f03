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
    call test_properties()

contains
    subroutine test_copy()
        implicit none
        type(chfl_atom) :: atom, cloned
        integer :: status

        call atom%init('He', status=status)
        CHECK(status == CHFL_SUCCESS)
        call cloned%copy(atom, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(atom%name(status=status) == 'He')
        CHECK(status == CHFL_SUCCESS)

        CHECK(cloned%name(status=status) == 'He')
        CHECK(status == CHFL_SUCCESS)

        call atom%set_name('Zn', status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(atom%name(status=status) == 'Zn')
        CHECK(status == CHFL_SUCCESS)

        CHECK(cloned%name(status=status) == 'He')
        CHECK(status == CHFL_SUCCESS)

        call atom%free()
        ! Call free twice to check that it works
        call cloned%free()
        call cloned%free()
    end subroutine

    subroutine test_name()
        implicit none
        type(chfl_atom) :: atom
        integer :: status

        call atom%init('He', status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(atom%name(status=status) == 'He')
        CHECK(status == CHFL_SUCCESS)

        CHECK(atom%full_name(status=status) == 'Helium')
        CHECK(status == CHFL_SUCCESS)

        call atom%set_name('Zn', status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(atom%name(status=status) == 'Zn')
        CHECK(status == CHFL_SUCCESS)

        call atom%free()
    end subroutine

    subroutine test_type()
        implicit none
        type(chfl_atom) :: atom
        integer :: status

        call atom%init('He', status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(atom%type(status=status) == 'He')
        CHECK(status == CHFL_SUCCESS)

        call atom%set_type('Zn', status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(atom%type(status=status) == 'Zn')
        CHECK(status == CHFL_SUCCESS)

        CHECK(atom%full_name(status=status) == 'Zinc')
        CHECK(status == CHFL_SUCCESS)

        call atom%free()
    end subroutine

    subroutine test_mass()
        implicit none
        type(chfl_atom) :: atom
        integer :: status

        call atom%init('He', status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(atom%mass(status=status) == 4.002602d0)
        CHECK(status == CHFL_SUCCESS)

        call atom%set_mass(678.0d0, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(atom%mass(status=status) == 678.0)
        CHECK(status == CHFL_SUCCESS)

        call atom%free()
    end subroutine

    subroutine test_charge()
        implicit none
        type(chfl_atom) :: atom
        integer :: status

        call atom%init('He', status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(atom%charge(status=status) == 0.0)
        CHECK(status == CHFL_SUCCESS)

        call atom%set_charge(-1.5d0, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(atom%charge(status=status) == -1.5)
        CHECK(status == CHFL_SUCCESS)

        call atom%free()
    end subroutine

    subroutine test_radius()
        implicit none
        type(chfl_atom) :: atom
        integer :: status

        call atom%init('Zn', status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(atom%vdw_radius(status=status) == 2.1d0)
        CHECK(status == CHFL_SUCCESS)

        CHECK(atom%covalent_radius(status=status) == 1.31d0)
        CHECK(status == CHFL_SUCCESS)

        call atom%free()
    end subroutine

    subroutine test_atomic_number()
        implicit none
        type(chfl_atom) :: atom
        integer :: status

        call atom%init('Zn', status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(atom%atomic_number(status=status) == 30)
        CHECK(status == CHFL_SUCCESS)

        call atom%free()
    end subroutine

    subroutine test_properties()
        implicit none
        type(chfl_atom) :: atom
        type(chfl_property) :: property
        character(len=CHFL_STRING_LENGTH), dimension(2) :: names
        integer :: status

        call atom%init('He', status=status)
        CHECK(status == CHFL_SUCCESS)

        call property%init(42d0, status=status)
        CHECK(status == CHFL_SUCCESS)

        call atom%set("foo", property, status=status)
        CHECK(status == CHFL_SUCCESS)
        call atom%set("bar", .false., status=status)
        CHECK(status == CHFL_SUCCESS)

        call property%free()

        property = atom%get("foo", status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(property%double(status=status) == 42d0)
        CHECK(status == CHFL_SUCCESS)
        call property%free()

        property = atom%get("bar", status=status)
        CHECK(status == CHFL_SUCCESS)
        CHECK(property%bool(status=status) .eqv. .false.)
        CHECK(status == CHFL_SUCCESS)
        call property%free()

        property = atom%get("baz", status=status)
        CHECK(status /= CHFL_SUCCESS)

        CHECK(atom%properties_count(status=status) == 2)
        CHECK(status == CHFL_SUCCESS)

        call atom%list_properties(names, status=status)
        CHECK(status == CHFL_SUCCESS)

        CHECK(names(1) == 'bar')
        CHECK(names(2) == 'foo')

        call atom%free()
    end subroutine
end program
