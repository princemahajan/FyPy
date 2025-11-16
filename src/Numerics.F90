!############################################################################################
!
!  FyPy
!
!> \brief       Numerics module
!! \details     It provides Numeric object protocol of Python/C API
!! \author      Bharat Mahajan
!! \date        08/11/2019    
!
!############################################################################################


module Numerics

    use, intrinsic :: iso_c_binding, only: C_INT, C_LONG, C_LONG_LONG, C_DOUBLE, C_PTR

    implicit none

    public

    !> Py_Comlex struct of Python/C for representing complex numbers 
    type, bind(C) :: Py_Complex
        real(C_DOUBLE) :: rpart
        real(C_DOUBLE) :: ipart
    end type Py_Complex

    interface

    !###################
    ! Integer Object API
    !###################

    !> Convert long int to PyObject long type
    type (C_PTR) function PyLong_FromLong(longint) &
                            bind(C,name="PyLong_FromLong")
        import :: C_PTR, C_LONG
        implicit none
        integer(kind=C_LONG), value :: longint
    end function PyLong_FromLong

    !> Convert long long int to PyObject long type
    type (C_PTR) function PyLong_FromLongLong(longlongint) &
                            bind(C,name="PyLong_FromLongLong")
        import :: C_PTR, C_LONG_LONG
        implicit none
        integer(kind=C_LONG_LONG), value :: longlongint
    end function PyLong_FromLongLong

    !> See Integer objects Python/C API: Case PyObject to long long int
    integer(C_LONG) function PyLong_AsLong(obj) &
                            bind(C,name="PyLong_AsLong")
        import :: C_PTR, C_LONG
        implicit none
        type(C_PTR), value :: obj
    end function PyLong_AsLong

    !> See Integer objects Python/C API: Case PyObject to long long int
    integer(C_LONG_LONG) function PyLong_AsLongLong(obj) &
                            bind(C,name="PyLong_AsLongLong")
        import :: C_PTR, C_LONG_LONG
        implicit none
        type(C_PTR), value :: obj
    end function PyLong_AsLongLong

    !> See Integer objects Python/C API: Case PyObject to long long int
    integer(C_LONG_LONG) function PyLong_AsLongLongAndOverflow(obj, overflow) &
                            bind(C,name="PyLong_AsLongLongAndOverflow")
        import :: C_PTR, C_LONG_LONG
        implicit none
        type(C_PTR), value :: obj
        type(C_PTR), value :: overflow    
    end function PyLong_AsLongLongAndOverflow


    !##########################
    ! Floating Point Object API
    !##########################


    !> See Floating point objects Python/C API: convert C double into float object
    type(C_PTR) function PyFloat_FromDouble(dblvar) bind(C, name="PyFloat_FromDouble")
        import :: C_PTR, C_DOUBLE
        implicit none
        real(kind=C_Double), value :: dblvar
    end function PyFloat_FromDouble

    !> See Floating point objects Python/C API: convert float object to C double
    real(kind=C_Double) function PyFloat_AsDouble(obj) bind(C, name="PyFloat_AsDouble")
        import :: C_PTR, C_DOUBLE
        implicit none
        type(C_PTR), value :: obj
    end function PyFloat_AsDouble


    !####################
    ! Boolean objects API
    !####################


    !> Convert long int to Py_True or Py_False object
    type (C_PTR) function PyBool_FromLong(longint) &
                            bind(C,name="PyBool_FromLong")
        import :: C_PTR, C_LONG
        implicit none
        integer(kind=C_LONG), value :: longint
    end function PyBool_FromLong

    integer(C_INT) function PyObject_IsTrue(obj) bind(C, name = "PyObject_IsTrue")
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: obj
    end function PyObject_IsTrue


    !####################
    ! Complex objects API
    !####################


    integer(C_INT) function PyComplex_Check(obj) bind(C,name="PyComplex_Check")
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: obj
    end function PyComplex_Check

    integer(C_INT) function PyComplex_CheckExact(obj) bind(C,name="PyComplex_CheckExact")
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: obj
    end function PyComplex_CheckExact


    type (C_PTR) function PyComplex_FromDoubles(dblreal, dblimag) &
                            bind(C,name="PyComplex_FromDoubles")
        import :: C_PTR, C_DOUBLE
        implicit none
        real(kind=C_DOUBLE), value :: dblreal
        real(kind=C_DOUBLE), value :: dblimag
    end function PyComplex_FromDoubles

    type(Py_Complex) function PyComplex_AsCComplex(obj) bind(C, name="PyComplex_AsCComplex")
        import :: C_PTR, Py_Complex
        implicit none
        type(C_PTR), value :: obj
    end function PyComplex_AsCComplex


    end interface



    contains



end module Numerics

