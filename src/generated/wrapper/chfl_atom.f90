! Chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2017 Guillaume Fraux -- BSD licence
!
! =========================================================================== !
! !!!! AUTO-GENERATED FILE !!!! Do not edit. See bindgen repository for the
! generating code (https://github.com/chemfiles/bindgen).
! This file contains Fortran 2003 ISO C Binding interface to the C API
!
! This file is not compilable on his own, but should be 'include'd in another
! fortran compilation unit.
! =========================================================================== !


subroutine chfl_atom_init_(this, name, status)
    implicit none
    class(chfl_atom) :: this
    character(len=*), intent(in) :: name
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_atom(f_to_c_str(name))

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_copy_init_(this, atom, status)
    implicit none
    class(chfl_atom) :: this
    class(chfl_atom), intent(in) :: atom
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_atom_copy(atom%ptr)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_from_frame_init_(this, frame, index, status)
    implicit none
    class(chfl_atom) :: this
    class(chfl_frame), intent(in) :: frame
    integer(kind=c_int64_t), value :: index
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_atom_from_frame(frame%ptr, index)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_from_topology_init_(this, topology, index, status)
    implicit none
    class(chfl_atom) :: this
    class(chfl_topology), intent(in) :: topology
    integer(kind=c_int64_t), value :: index
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_atom_from_topology(topology%ptr, index)

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_mass(this, mass, status)
    implicit none
    class(chfl_atom), intent(in) :: this
    real(kind=c_double) :: mass
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_atom_mass(this%ptr, mass)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_set_mass(this, mass, status)
    implicit none
    class(chfl_atom) :: this
    real(kind=c_double), value :: mass
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_atom_set_mass(this%ptr, mass)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_charge(this, charge, status)
    implicit none
    class(chfl_atom), intent(in) :: this
    real(kind=c_double) :: charge
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_atom_charge(this%ptr, charge)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_set_charge(this, charge, status)
    implicit none
    class(chfl_atom) :: this
    real(kind=c_double), value :: charge
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_atom_set_charge(this%ptr, charge)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_type(this, type, buffsize, status)
    implicit none
    class(chfl_atom), intent(in) :: this
    character(len=*) :: type
    integer(kind=c_int64_t), value :: buffsize
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_atom_type(this%ptr, type, buffsize)
    type = rm_null_in_str(type)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_set_type(this, type, status)
    implicit none
    class(chfl_atom) :: this
    character(len=*), intent(in) :: type
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_atom_set_type(this%ptr, f_to_c_str(type))
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_name(this, name, buffsize, status)
    implicit none
    class(chfl_atom), intent(in) :: this
    character(len=*) :: name
    integer(kind=c_int64_t), value :: buffsize
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_atom_name(this%ptr, name, buffsize)
    name = rm_null_in_str(name)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_set_name(this, name, status)
    implicit none
    class(chfl_atom) :: this
    character(len=*), intent(in) :: name
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_atom_set_name(this%ptr, f_to_c_str(name))
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_full_name(this, name, buffsize, status)
    implicit none
    class(chfl_atom), intent(in) :: this
    character(len=*) :: name
    integer(kind=c_int64_t), value :: buffsize
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_atom_full_name(this%ptr, name, buffsize)
    name = rm_null_in_str(name)
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_vdw_radius(this, radius, status)
    implicit none
    class(chfl_atom), intent(in) :: this
    real(kind=c_double) :: radius
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_atom_vdw_radius(this%ptr, radius)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_covalent_radius(this, radius, status)
    implicit none
    class(chfl_atom), intent(in) :: this
    real(kind=c_double) :: radius
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_atom_covalent_radius(this%ptr, radius)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_atomic_number(this, number, status)
    implicit none
    class(chfl_atom), intent(in) :: this
    integer(kind=c_int64_t) :: number
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_atom_atomic_number(this%ptr, number)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_set_property(this, name, property, status)
    implicit none
    class(chfl_atom) :: this
    character(len=*), intent(in) :: name
    class(chfl_property), intent(in) :: property
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_atom_set_property(this%ptr, f_to_c_str(name), property%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_get_property_init_(this, atom, name, status)
    implicit none
    class(chfl_property) :: this
    class(chfl_atom), intent(in) :: atom
    character(len=*), intent(in) :: name
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    this%ptr = c_chfl_atom_get_property(atom%ptr, f_to_c_str(name))

    if (.not. c_associated(this%ptr)) then
        status_tmp_ = CHFL_MEMORY_ERROR
    else
        status_tmp_ = CHFL_SUCCESS
    end if

    
    if (present(status)) then
        status = status_tmp_
    end if
end subroutine

subroutine chfl_atom_free(this, status)
    implicit none
    class(chfl_atom) :: this
    integer(chfl_status), optional :: status
    integer(chfl_status) :: status_tmp_

    status_tmp_ = c_chfl_atom_free(this%ptr)
    
    if (present(status)) then
        status = status_tmp_
    end if
    this%ptr = c_null_ptr
end subroutine
