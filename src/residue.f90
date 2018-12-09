! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux -- BSD licence
module chemfiles_residue
    use iso_c_binding
    use chemfiles_cdef
    use chemfiles_strings
    implicit none

    private
    public :: chfl_residue

    type chfl_residue
        private
        type(c_ptr) :: ptr = c_null_ptr
        logical :: is_const = .false.
    contains
        procedure :: unsafe_set_const_ptr
        procedure :: unsafe_set_ptr
        procedure :: unsafe_const_ptr
        procedure :: unsafe_ptr

        generic :: init => create, copy
        procedure, private :: create, copy

        procedure :: atoms_count
        procedure :: atoms
        procedure :: id
        procedure :: name
        procedure :: add_atom
        procedure :: contains
        procedure :: free
        ! procedure :: properties_count => chfl_residue_properties_count
        ! procedure :: list_properties => chfl_residue_list_properties
        ! procedure :: set_property => chfl_residue_set_property
        ! procedure :: get_property => chfl_residue_get_property_init_
    end type

contains
    subroutine unsafe_set_const_ptr(this, ptr, status)
        implicit none
        class(chfl_residue), intent(inout) :: this
        type(c_ptr), intent(in) :: ptr
        integer(chfl_status), intent(out), optional :: status

        call this%unsafe_set_ptr(ptr, status)
        this%is_const = .true.
    end subroutine

    subroutine unsafe_set_ptr(this, ptr, status)
        implicit none
        class(chfl_residue), intent(inout) :: this
        type(c_ptr), intent(in) :: ptr
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: dummy

        if (c_associated(this%ptr)) then
            write(*, *) "Trying to reset an allocated chfl_residue. Call chfl_residue%free first."
            ! free the allocated memory
            dummy = c_chfl_residue_free(ptr)
            if (present(status)) then
                status = CHFL_MEMORY_ERROR
            end if
            return
        end if

        this%ptr = ptr

        if (present(status)) then
            if (.not. c_associated(this%ptr)) then
                status = CHFL_MEMORY_ERROR
            else
                status = CHFL_SUCCESS
            end if
        end if
    end subroutine

    type(c_ptr) function unsafe_ptr(this)
        implicit none
        class(chfl_residue), intent(inout) :: this

        if (.not. c_associated(this%ptr)) then
            write(*, *) "Trying to access a NULL chfl_residue. Call chfl_residue%init first."
            stop 1
        elseif (this%is_const) then
            write(*, *) "Can not write data to a const chfl_residue"
            stop 1
        end if
        unsafe_ptr = this%ptr
    end function

    type(c_ptr) function unsafe_const_ptr(this)
        implicit none
        class(chfl_residue), intent(in) :: this

        if (.not. c_associated(this%ptr)) then
            write(*, *) "Trying to access a NULL chfl_residue. Call chfl_residue%init first."
            stop 1
        end if
        unsafe_const_ptr = this%ptr
    end function

    subroutine create(this, name, resid, status)
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

    ! subroutine properties_count(this, count, status)
    !     implicit none
    !     class(chfl_residue), intent(in) :: this
    !     integer(kind=c_int64_t) :: count
    !     integer(chfl_status), intent(out), optional :: status
    !     integer(chfl_status) :: status_tmp
    !
    !     status_tmp = c_chfl_residue_properties_count(this%unsafe_ptr(), count)
    !
    !     if (present(status)) then
    !         status = status_tmp
    !     end if
    ! end subroutine

    ! subroutine list_properties(this, names, count, status)
    !     implicit none
    !     class(chfl_residue), intent(in) :: this
    !     character, intent(in), dimension(:, :), target :: names
    !     integer(kind=c_int64_t), intent(in) :: count
    !     integer(chfl_status), intent(out), optional :: status
    !     integer(chfl_status) :: status_tmp
    !
    !     status_tmp = c_chfl_residue_list_properties(this%unsafe_ptr(), c_loc(names), count)
    !
    !     if (present(status)) then
    !         status = status_tmp
    !     end if
    ! end subroutine

    ! subroutine set_property(this, name, property, status)
    !     implicit none
    !     class(chfl_residue), intent(inout) :: this
    !     character(len=*), intent(in) :: name
    !     class(chfl_property), intent(in) :: property
    !     integer(chfl_status), intent(out), optional :: status
    !     integer(chfl_status) :: status_tmp
    !
    !     status_tmp = c_chfl_residue_set_property(this%unsafe_ptr(), f_to_c_str(name), property%unsafe_ptr())
    !
    !     if (present(status)) then
    !         status = status_tmp
    !     end if
    ! end subroutine
    !
    ! subroutine get_property_init_(this, residue, name, status)
    !     implicit none
    !     class(chfl_property) :: this
    !     class(chfl_residue), intent(in) :: residue
    !     character(len=*), intent(in) :: name
    !     integer(chfl_status), intent(out), optional :: status
    !     integer(chfl_status) :: status_tmp
    !
    !     this%unsafe_ptr() = c_chfl_residue_get_property(residue%unsafe_ptr(), f_to_c_str(name))
    !
    !     if (.not. c_associated(this%unsafe_ptr())) then
    !         status_tmp = CHFL_MEMORY_ERROR
    !     else
    !         status_tmp = CHFL_SUCCESS
    !     end if
    !
    !
    !     if (present(status)) then
    !         status = status_tmp
    !     end if
    ! end subroutine

    subroutine free(this, status)
        implicit none
        class(chfl_residue), intent(inout) :: this
        integer(chfl_status), intent(out), optional :: status
        integer(chfl_status) :: status_tmp

        status_tmp = c_chfl_residue_free(this%ptr)
        this%ptr = c_null_ptr
        this%is_const = .false.

        if (present(status)) then
            status = status_tmp
        end if
    end subroutine
end module
