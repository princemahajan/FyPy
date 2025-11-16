!############################################################################################
!
!  FyPy
!
!> \brief       Exception Handling module
!! \details     It provides Fortran interface to Python/C API for Exception Handling.
!! \author      Bharat Mahajan
!! \date        08/11/2019    
!
!############################################################################################


module Exceptions

    use, intrinsic :: iso_c_binding, only: C_CHAR, C_LONG, C_PTR

    implicit none

    public

    !> wrong argument type exception
    type(C_PTR), save :: PyExc_TypeError

    !> module import error
    type(C_PTR), save :: PyExc_ImportError



    
    interface

    !> check for any exception raised
    type(C_PTR) function PyErr_Occurred() bind(C,name="PyErr_Occurred")
        import :: C_PTR
        implicit none
    end function

    
    
    !> Set error object without the associated string
    subroutine PyErr_SetNone(errtype) bind(C, name="PyErr_SetNone")
        import :: C_PTR
        implicit none
        type(C_PTR), value :: errtype
    end subroutine PyErr_SetNone

    
    !> Set error object and the associated string
    subroutine PyErr_SetString(errtype, message) bind(C, name="PyErr_SetString")
        import :: C_PTR, C_CHAR
        implicit none
        type(C_PTR), value :: errtype
        character(kind=C_CHAR,len=1), dimension(*) :: message
    end subroutine PyErr_SetString

    
    end interface



    contains



end module Exceptions

