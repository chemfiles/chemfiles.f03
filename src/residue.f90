! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_residue
    use iso_c_binding
    use chemfiles_ffi
    use chemfiles_strings
    use chemfiles_property
    implicit none

    private
    public :: chfl_residue

    type, extends(chfl_ptr) :: chfl_residue
    contains
        procedure :: init
        procedure :: copy
        procedure :: atoms_count
        procedure :: atoms
        procedure :: id
        procedure :: name
        procedure :: add_atom
        procedure :: contains

        generic :: set => set_property, set_bool, set_double, set_string, set_vector3d
        procedure, private :: set_property, set_bool, set_double, set_string, set_vector3d
        procedure :: properties_count
        procedure :: list_properties
        procedure :: get
    end type

contains
    subroutine init(this, name, resid, status)
        implicit none
        class(chfl_residue), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(kind=c_int64_t), optional :: resid
        integer(chfl_status), intent(out), optional :: status

        if (present(resid)) then
            call this%unsafe_set_ptr(c_chfl_residue_with_id(f_to_c_str(name), resid), status)
        else
            call this%unsafe_set_ptr(c_chfl_residue(f_to_c_str(name)), status)
        end if
    end subroutine

    subroutine copy(this, residue, status)
        implicit none
        class(chfl_residue), intent(inout) :: this
        class(chfl_residue), intent(in) :: residue
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(c_chfl_residue_copy(residue%unsafe_const_ptr()), status)
    end subroutine

    integer(kind=c_int64_t) function atoms_count(this, status)
        implicit none
        class(chfl_residue), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_residue_atoms_count(this%unsafe_const_ptr(), atoms_count)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine atoms(this, data, status)
        implicit none
        class(chfl_residue), intent(in) :: this
        integer(kind=c_int64_t), intent(out), dimension(:), target :: data
        integer(chfl_status), intent(out), optional :: status

        integer(chfl_status) :: status_tmp
        integer(kind=c_int64_t) :: count

        count = size(data, 1, c_int64_t)
        status_tmp = c_chfl_residue_atoms(this%unsafe_const_ptr(), c_loc(data), count)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    integer(kind=c_int64_t) function id(this, status)
        implicit none
        class(chfl_residue), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_residue_id(this%unsafe_const_ptr(), id)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    character(len=CHFL_STRING_LENGTH) function name(this, status)
        implicit none
        class(chfl_residue), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_residue_name(this%unsafe_const_ptr(), name, int(CHFL_STRING_LENGTH, c_int64_t))
        name = rm_null_in_str(name)
        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine add_atom(this, index, status)
        implicit none
        class(chfl_residue), intent(inout) :: this
        integer(kind=c_int64_t), intent(in) :: index
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_residue_add_atom(this%unsafe_ptr(), index)

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    logical function contains(this, index, status)
        implicit none
        class(chfl_residue), intent(in) :: this
        integer(kind=c_int64_t), intent(in) :: index
        integer(chfl_status), intent(out), optional :: status

        integer(chfl_status) :: status_tmp
        logical(c_bool) :: result

        status_tmp = c_chfl_residue_contains(this%unsafe_const_ptr(), index, result)
        contains = logical(result)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine set_property(this, name, property, status)
        implicit none
        class(chfl_residue), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(chfl_property), intent(in) :: property
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_residue_set_property(                                &
            this%unsafe_ptr(), f_to_c_str(name), property%unsafe_const_ptr()  &
        )

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine

    subroutine set_bool(this, name, value, status)
        implicit none
        class(chfl_residue), intent(inout) :: this
        character(len=*), intent(in) :: name
        logical, intent(in) :: value
        integer(chfl_status), intent(out), optional :: status

        type(chfl_property) :: property
        integer(chfl_status) :: status_tmp

        call property%init(value, status=status_tmp)
        if (status_tmp /= CHFL_SUCCESS) then
            if (present(status)) then
                status = status_tmp
            end if
            return
        end if

        call this%set_property(name, property, status=status)
        call property%free()
    end subroutine

    subroutine set_double(this, name, value, status)
        implicit none
        class(chfl_residue), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(c_double), intent(in) :: value
        integer(chfl_status), intent(out), optional :: status

        type(chfl_property) :: property
        integer(chfl_status) :: status_tmp

        call property%init(value, status=status_tmp)
        if (status_tmp /= CHFL_SUCCESS) then
            if (present(status)) then
                status = status_tmp
            end if
            return
        end if

        call this%set_property(name, property, status=status)
        call property%free()
    end subroutine

    subroutine set_string(this, name, value, status)
        implicit none
        class(chfl_residue), intent(inout) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: value
        integer(chfl_status), intent(out), optional :: status

        type(chfl_property) :: property
        integer(chfl_status) :: status_tmp

        call property%init(value, status=status_tmp)
        if (status_tmp /= CHFL_SUCCESS) then
            if (present(status)) then
                status = status_tmp
            end if
            return
        end if

        call this%set_property(name, property, status=status)
        call property%free()
    end subroutine

    subroutine set_vector3d(this, name, value, status)
        implicit none
        class(chfl_residue), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(c_double), dimension(3), intent(in) :: value
        integer(chfl_status), intent(out), optional :: status

        type(chfl_property) :: property
        integer(chfl_status) :: status_tmp

        call property%init(value, status=status_tmp)
        if (status_tmp /= CHFL_SUCCESS) then
            if (present(status)) then
                status = status_tmp
            end if
            return
        end if

        call this%set_property(name, property, status=status)
        call property%free()
    end subroutine

    type(chfl_property) function get(this, name, status)
        implicit none
        class(chfl_residue), intent(in) :: this
        character(len=*), intent(in) :: name
        integer(chfl_status), intent(out), optional :: status

        call get%unsafe_set_ptr(                                                 &
            c_chfl_residue_get_property(this%unsafe_const_ptr(), f_to_c_str(name)), &
            status                                                               &
        )
    end function

    integer(kind=c_int64_t) function properties_count(this, status)
        implicit none
        class(chfl_residue), intent(in) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_residue_properties_count(this%unsafe_const_ptr(), properties_count)

        if (present(status)) then
            status = status_tmp
        end if
    end function

    subroutine list_properties(this, names, status)
        implicit none
        class(chfl_residue), intent(in) :: this
        character(len=CHFL_STRING_LENGTH), intent(out), dimension(:) :: names
        integer(chfl_status), intent(out), optional :: status

        type(c_ptr), dimension(:), allocatable, target :: c_names
        integer(kind=c_int64_t) :: count, i
        integer(chfl_status) :: status_tmp

        count = size(names, 1, c_int64_t)
        allocate(c_names(count))

        status_tmp = c_chfl_residue_list_properties(this%unsafe_const_ptr(), c_loc(c_names), count)
        if (status_tmp /= CHFL_SUCCESS) goto 1000

        do i = 1, count
            names(i) = c_to_f_str(c_names(i))
        end do

1000    deallocate(c_names)
        if (present(status)) then
            status = status_tmp
        end if
    end subroutine
end module
