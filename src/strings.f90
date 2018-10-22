! chemfiles, an efficient IO library for chemistry file formats
! Copyright (C) 2015-2019 Guillaume Fraux - BSD License

module chemfiles_strings
    use iso_c_binding
    implicit none
    private
    public c_to_f_str, rm_null_in_str, f_to_c_str, CHFL_STRING_LENGTH
    integer, parameter :: CHFL_STRING_LENGTH = 1024

    interface rm_null_in_str
        module procedure rm_null_in_c_str, rm_null_in_f_str
    end interface

contains
    !** Convert a C pointer (char*) to a Fortran string
    function c_to_f_str(c_string) result(f_string)
        implicit none
        type(c_ptr), target, intent(in) :: c_string
        character(len=1), dimension(:), pointer :: tmp
        character(len=CHFL_STRING_LENGTH) :: f_string

        call c_f_pointer(c_string, tmp, shape=[CHFL_STRING_LENGTH])
        f_string = rm_null_in_str(tmp)
    end function

    !** Convert a C_NULL terminated C string to a Fortran string
    function rm_null_in_c_str(c_string) result(f_string)
        implicit none
        character(len=1), dimension(:), intent(in) :: c_string
        character(len=CHFL_STRING_LENGTH) :: f_string
        integer :: str_len, i

        str_len = 1
        do while (c_string(str_len) /= c_null_char)
            str_len = str_len + 1
        end do
        str_len = str_len - 1

        do i=1, str_len
            f_string(i:i) = c_string(i)
        end do
        do i=str_len+1, CHFL_STRING_LENGTH
            f_string(i:i) = " "
        end do
    end function

    !** Convert a C_NULL terminated Fortran string to a Fortran string
    function rm_null_in_f_str(null_string) result(f_string)
        implicit none
        character(len=*), intent(in) :: null_string
        character(len=len_trim(null_string)) :: f_string
        integer :: str_len, i

        str_len = index(null_string, c_null_char) - 1
        do i=1, str_len
            f_string(i:i) = null_string(i:i)
        end do
        do i=str_len+1, len_trim(null_string)
            f_string(i:i) = " "
        end do
    end function

    !** Convert a Fortran string to a C string
    function f_to_c_str(f_string) result(c_string)
        implicit none
        character(len=*), intent(in) :: f_string
        character(len=1, kind=c_char) :: c_string(len_trim(f_string)+1)
        integer :: N, i

        N = len_trim(f_string)
        do i = 1, N
            c_string(i) = f_string(i:i)
        end do
        c_string(n + 1) = c_null_char
    end function
end module
