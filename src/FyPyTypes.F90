!############################################################################################
!
!  FyPy
!
!> \brief       FyPyTypes submodule
!! \details     It provides type conversion and related procedures between Fortran and 
!!              Python/C API objects
!! \author      Bharat Mahajan
!! \date        08/11/2019    
!
!############################################################################################


submodule (FyPy) FyPyTypes


    

    contains

    
    
    
    
    !#########################
    ! Check for PyObject Types
    !#########################

    !> see longobject.h for PyLong_Check and PyLong_CheckExact
    logical module function IsPyLongType(obj, CheckExactType)
        implicit none
        type(C_PTR), value :: obj
        logical, intent(in), optional :: CheckExactType

        logical :: CheckForExactType
        type(PyObject), pointer :: pObj
        type(PyTypeObject), pointer :: pTypeObj

        if (present(CheckExactType)) then
            CheckForExactType = CheckExactType
        else
            CheckForExactType = .False.
        end if

        ! get the pointer to type object
        call c_f_pointer(obj, pObj)
        call c_f_pointer(pobj%ob_type, pTypeObj)

        IsPyLongType = .FALSE.

        if (CheckForExactType) then
            ! true only if given object is of the same type
            if (c_associated(pobj%ob_type, Py_Long_TypeObject)) IsPyLongType = .TRUE.
        else
            ! true if given object is of the same type or subtype
            if(iand(pTypeObj%tp_flags, Py_TPFLAGS_LONG_SUBCLASS) /= 0) IsPyLongType = .TRUE.
        end if

    end function IsPyLongType


    !> see floatobject.h for PyFloat_Check and PyFloat_CheckExact
    logical module function IsPyFloatType(obj, CheckExactType)
        implicit none
        type(C_PTR), value :: obj
        logical, intent(in), optional :: CheckExactType

        logical :: CheckForExactType, ObjIsExactType
        type(PyObject), pointer :: pObj
        type(PyTypeObject), pointer :: pTypeObj

        if (present(CheckExactType)) then
            CheckForExactType = CheckExactType
        else
            CheckForExactType = .False.
        end if

        ! get the pointer to type object
        call c_f_pointer(obj, pObj)
        call c_f_pointer(pobj%ob_type, pTypeObj)

        ! check for exact type
        ObjIsExactType = .FALSE.
        if (c_associated(pobj%ob_type, Py_Float_TypeObject)) ObjIsExactType = .TRUE.

        IsPyFloatType = .FALSE.

        if (CheckForExactType) then
            ! true only if given object is of the exact same type
            if (ObjIsExactType) IsPyFloatType = .TRUE.
        else
            ! true if given object is of the same type or subtype
            if(ObjIsExactType .OR.  PyType_IsSubType(pobj%ob_type, Py_Float_TypeObject) /= 0) &
                        IsPyFloatType = .TRUE.
        end if

    end function IsPyFloatType



    !> see complexobject.h
    logical module function IsPyComplexType(obj, CheckExactType)
        implicit none
        type(C_PTR), value :: obj
        logical, intent(in), optional :: CheckExactType

        logical :: CheckForExactType
        type(PyObject), pointer :: pObj
        type(PyTypeObject), pointer :: pTypeObj

        logical :: ObjIsExactType

        if (present(CheckExactType)) then
            CheckForExactType = CheckExactType
        else
            CheckForExactType = .False.
        end if

        ! get the pointer to type object
        call c_f_pointer(obj, pObj)
        call c_f_pointer(pobj%ob_type, pTypeObj)

        ! check for exact type
        ObjIsExactType = .FALSE.
        if (c_associated(pobj%ob_type, Py_Complex_TypeObject)) ObjIsExactType = .TRUE.

        IsPyComplexType = .FALSE.

        if (CheckForExactType) then
            ! true only if given object is of the exact same type
            if (ObjIsExactType) IsPyComplexType = .TRUE.
        else
            ! true if given object is of the same type or subtype
            if(ObjIsExactType .OR.  PyType_IsSubType(pobj%ob_type, Py_Complex_TypeObject) /= 0) &
                IsPyComplexType = .TRUE.
        end if

    end function IsPyComplexType




    !> see boolobject.h for PyBool_Check
    logical module function IsPyBoolType(obj)
        implicit none
        type(C_PTR), value :: obj

        type(PyObject), pointer :: pObj
        type(PyTypeObject), pointer :: pTypeObj

        ! get the pointer to type object
        call c_f_pointer(obj, pObj)
        call c_f_pointer(pobj%ob_type, pTypeObj)

        IsPyBoolType = .FALSE.

        ! true only if given object is of the same type
        if (c_associated(pobj%ob_type, Py_Bool_TypeObject)) IsPyBoolType = .TRUE.
    end function IsPyBoolType






    !> see longobject.h for PyLong_Check and PyLong_CheckExact
    logical module function IsPyUnicodeType(obj, CheckExactType)
        implicit none
        type(C_PTR), value :: obj
        logical, intent(in), optional :: CheckExactType

        logical :: CheckForExactType
        type(PyObject), pointer :: pObj
        type(PyTypeObject), pointer :: pTypeObj

        if (present(CheckExactType)) then
            CheckForExactType = CheckExactType
        else
            CheckForExactType = .False.
        end if

        ! get the pointer to type object
        call c_f_pointer(obj, pObj)
        call c_f_pointer(pobj%ob_type, pTypeObj)

        IsPyUnicodeType = .FALSE.

        if (CheckForExactType) then
            ! true only if given object is of the same type
            if (c_associated(pobj%ob_type, Py_Unicode_TypeObject)) IsPyUnicodeType = .TRUE.
        else
            ! true if given object is of the same type or subtype
            if(iand(pTypeObj%tp_flags, Py_TPFLAGS_UNICODE_SUBCLASS) /= 0) IsPyUnicodeType = .TRUE.
        end if
    end function IsPyUnicodeType



    !> see tupleobject.h for PyTuple_Check and PyTuple_CheckExact
    logical module function IsPyTupleType(obj, CheckExactType)
        implicit none
        type(C_PTR), value :: obj
        logical, intent(in), optional :: CheckExactType

        logical :: CheckForExactType
        type(PyObject), pointer :: pObj
        type(PyTypeObject), pointer :: pTypeObj

        if (present(CheckExactType)) then
            CheckForExactType = CheckExactType
        else
            CheckForExactType = .False.
        end if

        ! get the pointer to type object
        call c_f_pointer(obj, pObj)
        call c_f_pointer(pobj%ob_type, pTypeObj)

        IsPyTupleType = .FALSE.

        if (CheckForExactType) then
            ! true only if given object is of the same type
            if (c_associated(pobj%ob_type, Py_Tuple_TypeObject)) IsPyTupleType = .TRUE.
        else
            ! true if given object is of the same type or subtype
            if(iand(pTypeObj%tp_flags, Py_TPFLAGS_TUPLE_SUBCLASS) /= 0) IsPyTupleType = .TRUE.
        end if
    end function IsPyTupleType



    !> see listobject.h for PyList_Check and PyList_CheckExact
    logical module function IsPyListType(obj, CheckExactType)
        implicit none
        type(C_PTR), value :: obj
        logical, intent(in), optional :: CheckExactType

        logical :: CheckForExactType
        type(PyObject), pointer :: pObj
        type(PyTypeObject), pointer :: pTypeObj

        if (present(CheckExactType)) then
            CheckForExactType = CheckExactType
        else
            CheckForExactType = .False.
        end if

        ! get the pointer to type object
        call c_f_pointer(obj, pObj)
        call c_f_pointer(pobj%ob_type, pTypeObj)

        IsPyListType = .FALSE.

        if (CheckForExactType) then
            ! true only if given object is of the same type
            if (c_associated(pobj%ob_type, Py_List_TypeObject)) IsPyListType = .TRUE.
        else
            ! true if given object is of the same type or subtype
            if(iand(pTypeObj%tp_flags, Py_TPFLAGS_LIST_SUBCLASS) /= 0) IsPyListType = .TRUE.
        end if
    end function IsPyListType




    !> see dictobject.h for PyDict_Check and PyDict_CheckExact
    logical module function IsPyDictType(obj, CheckExactType)
        implicit none
        type(C_PTR), value :: obj
        logical, intent(in), optional :: CheckExactType

        logical :: CheckForExactType
        type(PyObject), pointer :: pObj
        type(PyTypeObject), pointer :: pTypeObj

        if (present(CheckExactType)) then
            CheckForExactType = CheckExactType
        else
            CheckForExactType = .False.
        end if

        ! get the pointer to type object
        call c_f_pointer(obj, pObj)
        call c_f_pointer(pobj%ob_type, pTypeObj)

        IsPyDictType = .FALSE.

        if (CheckForExactType) then
            ! true only if given object is of the same type
            if (c_associated(pobj%ob_type, Py_Dict_TypeObject)) IsPyDictType = .TRUE.
        else
            ! true if given object is of the same type or subtype
            if(iand(pTypeObj%tp_flags, Py_TPFLAGS_DICT_SUBCLASS) /= 0) IsPyDictType = .TRUE.
        end if
    end function IsPyDictType


    !> see ndarrayobject.h for PyArray_Check and PyArray_CheckExact (NumPy)
    logical module function IsPyArrayType(obj, CheckExactType)
        implicit none
        type(C_PTR), value :: obj
        logical, intent(in), optional :: CheckExactType

        logical :: CheckForExactType, ObjIsExactType
        type(PyObject), pointer :: pObj
        type(PyTypeObject), pointer :: pTypeObj

        if (present(CheckExactType)) then
            CheckForExactType = CheckExactType
        else
            CheckForExactType = .False.
        end if

        ! get the pointer to type object
        call c_f_pointer(obj, pObj)
        call c_f_pointer(pobj%ob_type, pTypeObj)

        ! check for exact type
        ObjIsExactType = .FALSE.
        if (c_associated(pobj%ob_type, Py_Array_TypeObject)) ObjIsExactType = .TRUE.

        IsPyArrayType = .FALSE.

        if (CheckForExactType) then
            ! true only if given object is of the exact same type
            if (ObjIsExactType) IsPyArrayType = .TRUE.
        else
            ! true if given object is of the same type or subtype
            if(ObjIsExactType .OR.  PyType_IsSubType(pobj%ob_type, Py_Array_TypeObject) /= 0) &
                        IsPyArrayType = .TRUE.
        end if
    end function IsPyArrayType


    !#########################
    ! Integer Type Conversions
    !#########################

    !> Note long int can be equivalent to kind=4 or kind=8 in ifort 
    !! depending on the 32-bit or 64-bit mode for target architecture
    integer(kind(FYPY_SUCCESS)) module function Fy2PyLong(longintvar, obj)
        implicit none
        integer(C_LONG), intent(in) :: longintvar
        type(C_PTR), intent(out) :: obj

        ! Make the CPython API call
        obj = PyLong_FromLong(longintvar)

        ! check for exceptions
        if (.NOT. c_associated(obj)) then
            Fy2PyLong = FYPY_ERROR_PYTHONEXCEPTION
        else 
            Fy2PyLong = FYPY_SUCCESS
        end if
    end function Fy2PyLong


    integer(kind(FYPY_SUCCESS)) module function Fy2PyLongLong(longlongint, obj)
        implicit none
        integer(C_LONG_LONG), intent(in) :: longlongint
        type(C_PTR), intent(out) :: obj

        ! Make the CPython API call
        obj = PyLong_FromLongLong(longlongint)

        ! check for exceptions
        if (.NOT. c_associated(obj)) then
            Fy2PyLongLong = FYPY_ERROR_PYTHONEXCEPTION
        else 
            Fy2PyLongLong = FYPY_SUCCESS
        end if
    end function Fy2PyLongLong
    

    integer(kind(FYPY_SUCCESS)) module function Py2FyLong(obj, longintvar)
        implicit none
        type(C_PTR), value :: obj
        integer(C_LONG), intent(out) :: longintvar

        ! Make the CPython API call
        longintvar = PyLong_AsLong(obj)

        ! exception checking
        if (longintvar == -1 .AND. (c_associated(PyErr_Occurred()))) then
            Py2FyLong = FYPY_ERROR_PYTHONEXCEPTION
        else
            Py2FyLong = FYPY_SUCCESS
        end if
    end function Py2FyLong


    integer(kind(FYPY_SUCCESS)) module function Py2FyLongLong(obj, longlongint)
        implicit none
        type(C_PTR), value :: obj
        integer(C_LONG_LONG), intent(out) :: longlongint

        ! Make the CPython API call
        longlongint = PyLong_AsLongLong(obj)

        ! exception checking
        if (longlongint == -1 .AND. c_associated(PyErr_Occurred())) then
            Py2FyLongLong = FYPY_ERROR_PYTHONEXCEPTION
        else
            Py2FyLongLong = FYPY_SUCCESS
        end if
    end function Py2FyLongLong


    !###########################
    ! Floating-Point Conversions
    !###########################


    integer(kind(FYPY_SUCCESS)) module function Fy2PyDouble(dblvar, obj)
        implicit none
        real(C_DOUBLE), value :: dblvar
        type(C_PTR), intent(out) :: obj

        ! Make the CPython API call
        obj = PyFloat_FromDouble(dblvar)

        ! check for exceptions
        if (.NOT. c_associated(obj)) then
            Fy2PyDouble = FYPY_ERROR_PYTHONEXCEPTION
        else 
            Fy2PyDouble = FYPY_SUCCESS
        end if
    end function Fy2PyDouble
    

    integer(kind(FYPY_SUCCESS)) module function Py2FyDouble(obj, dblvar)
        implicit none        
        type(C_PTR), value :: obj
        real(C_DOUBLE), intent(out) :: dblvar

        ! Make the CPython API call
        dblvar = PyFloat_AsDouble(obj)

        ! exception checking
        if (dblvar == -1 .AND. c_associated(PyErr_Occurred())) then
            Py2FyDouble = FYPY_ERROR_PYTHONEXCEPTION
        else
            Py2FyDouble = FYPY_SUCCESS
        end if
    end function Py2FyDouble



    !########################
    ! Boolean type conversion
    !########################


    integer(kind(FYPY_SUCCESS)) module function Fy2PyBool(boolvar, obj)
        implicit none
        logical, intent(in) :: boolvar
        type(C_PTR), intent(out) :: obj

        if (boolvar) then
            obj = Py_True_Object
        else
            obj = Py_False_Object
        end if
        call Py_IncRef(obj)

        ! check for exceptions
        if (.NOT. c_associated(obj)) then
            Fy2PyBool = FYPY_ERROR_PYTHONEXCEPTION
        else 
            Fy2PyBool = FYPY_SUCCESS
        end if
    end function Fy2PyBool

    
    integer(kind(FYPY_SUCCESS)) module function Py2FyBool(obj, boolvar)
        implicit none
        logical, intent(out) :: boolvar
        type(C_PTR), value :: obj

        integer(C_INT) :: boolint

        boolint = PyObject_IsTrue(obj)

        if (boolint == -1) then
            Py2FyBool = FYPY_ERROR_PYTHONEXCEPTION
        else
            boolvar = (boolint == 1)
            Py2FyBool = FYPY_SUCCESS
        end if
    end function Py2FyBool


    !#########################
    ! Complex type conversions
    !#########################

    integer(kind(FYPY_SUCCESS)) module function Fy2PyComplex(cmplxvar, obj)
        implicit none
        complex(C_DOUBLE), intent(in) :: cmplxvar
        type(C_PTR), intent(out) :: obj

        obj = PyComplex_FromDoubles(real(cmplxvar), aimag(cmplxvar))

        ! check for exceptions
        if (.NOT. c_associated(obj)) then
            Fy2PyComplex = FYPY_ERROR_PYTHONEXCEPTION
        else 
            Fy2PyComplex = FYPY_SUCCESS
        end if
    end function Fy2PyComplex


    integer(kind(FYPY_SUCCESS)) module function Py2FyComplex(obj, cmplxvar)
        implicit none
        type(C_PTR), value :: obj
        complex(C_DOUBLE), intent(out) :: cmplxvar

        type(Py_Complex) :: tmp

        tmp = PyComplex_AsCComplex(obj)

        if (tmp%rpart == -1.0_C_DOUBLE .AND. (c_associated(PyErr_Occurred()))) then
            Py2FyComplex = FYPY_ERROR_PYTHONEXCEPTION
        else
            cmplxvar = cmplx(tmp%rpart, tmp%ipart, kind=C_DOUBLE)
            Py2FyComplex = FYPY_SUCCESS
        end if
    end function Py2FyComplex


    !###################
    ! String conversions
    !###################

    integer(kind(FYPY_SUCCESS)) module function Fy2PyString(strvar, obj)
        implicit none
        character(len=*), intent(in) :: strvar
        type(C_PTR), intent(out) :: obj

        ! call Python/C API
        obj = PyUnicode_DecodeUTF8(strvar, len(strvar,kind=PY_SSIZE_T), 'strict'//C_NULL_CHAR)

        ! check for exceptions
        if (.NOT. c_associated(obj)) then
            Fy2PyString = FYPY_ERROR_PYTHONEXCEPTION
        else 
            Fy2PyString = FYPY_SUCCESS
        end if
    end function Fy2PyString

    
    integer(kind(FYPY_SUCCESS)) module function Py2FyString(obj, strvar)
        implicit none
        type(C_PTR), value :: obj
        character(len=:), allocatable, intent(out) :: strvar

        character, dimension(:), pointer :: pstr
        type(C_PTR) :: cstr
        integer(PY_SSIZE_T) :: strlen, ctr

        ! get C string
        cstr = PyUnicode_AsUTF8AndSize(obj, strlen)

        ! check for exceptions
        if ((.NOT. c_associated(cstr)) .AND. c_associated(PyErr_Occurred())) then
            Py2FyString = FYPY_ERROR_PYTHONEXCEPTION
        else
            ! get the fortran string pointer
            call c_f_pointer(cstr, pstr,[strlen])
            
            ! allocate string
            allocate(character(len=strlen) :: strvar)
            
            do ctr = 1,strlen
                strvar(ctr:ctr) = pstr(ctr)
            end do
            
            Py2FyString = FYPY_SUCCESS
        end if
    end function Py2FyString


    !########################
    ! NumPy Array conversions
    !########################

    
    integer(kind(FYPY_SUCCESS)) module function Fy2PyArray(me, arr, arrtype, arrshape, pyarr)
        implicit none
        class(FyPyclass), intent(inout) :: me
        type(C_PTR), intent(in) :: arr
        integer(kind(FYPY_NUMPY_DTYPE_INT8)), intent(in) :: arrtype
        integer, dimension(:), intent(in) :: arrshape
        type(C_PTR), intent(out) :: pyarr

        ! fortran pointers for int8 data
        integer(C_INT8_T), dimension(:), pointer :: pi8r1data,pi8r1arr
        integer(C_INT8_T), dimension(:,:), pointer :: pi8r2data,pi8r2arr
        integer(C_INT8_T), dimension(:,:,:), pointer :: pi8r3data,pi8r3arr
        integer(C_INT8_T), dimension(:,:,:,:), pointer :: pi8r4data,pi8r4arr

       ! fortran pointers for int32 data
        integer(C_INT32_T), dimension(:), pointer :: pi32r1data,pi32r1arr
        integer(C_INT32_T), dimension(:,:), pointer :: pi32r2data,pi32r2arr
        integer(C_INT32_T), dimension(:,:,:), pointer :: pi32r3data,pi32r3arr
        integer(C_INT32_T), dimension(:,:,:,:), pointer :: pi32r4data,pi32r4arr

        ! fortran pointers for int64 data
        integer(C_INT64_T), dimension(:), pointer :: pi64r1data,pi64r1arr
        integer(C_INT64_T), dimension(:,:), pointer :: pi64r2data,pi64r2arr
        integer(C_INT64_T), dimension(:,:,:), pointer :: pi64r3data,pi64r3arr
        integer(C_INT64_T), dimension(:,:,:,:), pointer :: pi64r4data,pi64r4arr

        ! fortran pointers for real32 data
        real(C_FLOAT), dimension(:), pointer :: pr32r1data,pr32r1arr
        real(C_FLOAT), dimension(:,:), pointer :: pr32r2data,pr32r2arr
        real(C_FLOAT), dimension(:,:,:), pointer :: pr32r3data,pr32r3arr
        real(C_FLOAT), dimension(:,:,:,:), pointer :: pr32r4data,pr32r4arr

        ! fortran pointers for real64 data
        real(C_DOUBLE), dimension(:), pointer :: pr64r1data,pr64r1arr
        real(C_DOUBLE), dimension(:,:), pointer :: pr64r2data,pr64r2arr
        real(C_DOUBLE), dimension(:,:,:), pointer :: pr64r3data,pr64r3arr
        real(C_DOUBLE), dimension(:,:,:,:), pointer :: pr64r4data,pr64r4arr

        ! fortran pointers for complex64 data
        complex(C_DOUBLE_COMPLEX), dimension(:), pointer :: pc64r1data,pc64r1arr
        complex(C_DOUBLE_COMPLEX), dimension(:,:), pointer :: pc64r2data,pc64r2arr
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:), pointer :: pc64r3data,pc64r3arr
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:,:), pointer :: pc64r4data,pc64r4arr

        integer(kind(FYPY_SUCCESS))     :: status
        type(Py_Buffer)                 :: PyBuffer
        type(C_PTR)                     :: kwargs, obj, shapeobj, dtypeobj, orderobj
        integer(C_INT)                  :: error, flags
        integer(PY_SSIZE_T)             :: ctr, arrrank, typebytes

        nullify(pi8r1data,pi8r1arr,pi8r2data,pi8r2arr,pi8r3data,pi8r3arr,pi8r4data,pi8r4arr)
        nullify(pi32r1data,pi32r1arr,pi32r2data,pi32r2arr,pi32r3data,pi32r3arr,pi32r4data,pi32r4arr)
        nullify(pi64r1data,pi64r1arr,pi64r2data,pi64r2arr,pi64r3data,pi64r3arr,pi64r4data,pi64r4arr)
        nullify(pr32r1data,pr32r1arr,pr32r2data,pr32r2arr,pr32r3data,pr32r3arr,pr32r4data,pr32r4arr)
        nullify(pr64r1data,pr64r1arr,pr64r2data,pr64r2arr,pr64r3data,pr64r3arr,pr64r4data,pr64r4arr)
        nullify(pc64r1data,pc64r1arr,pc64r2data,pc64r2arr,pc64r3data,pc64r3arr,pc64r4data,pc64r4arr)

        ! reset
        Fy2PyArray = FYPY_SUCCESS
        pyarr = C_NULL_PTR

        ! set array shape
        arrrank = size(arrshape,kind=PY_SSIZE_T)
        shapeobj = PyTuple_New(arrrank)
        do ctr = 1, arrrank
            status = Fy2PyType(arrshape(ctr), obj)
            error = PyTuple_SetItem(shapeobj, ctr-1, obj)
            if (status /= FYPY_SUCCESS .OR. error /= 0) then
                call Py_Decref(shapeobj)
                Fy2PyArray = FYPY_ERROR_NUMPY_ARRAY_SHAPE
                return
            end if
        end do
    
        ! set data type  
        if (arrtype == FYPY_NUMPY_DTYPE_INT8) then
            dtypeobj = me%NumPy%NUMPY_DTYPE_INT8
            typebytes = 1
        elseif (arrtype == FYPY_NUMPY_DTYPE_INT32) then
            dtypeobj = me%NumPy%NUMPY_DTYPE_INT32
            typebytes = 4
        elseif (arrtype == FYPY_NUMPY_DTYPE_INT64) then
            dtypeobj = me%NumPy%NUMPY_DTYPE_INT64
            typebytes = 8
        elseif (arrtype == FYPY_NUMPY_DTYPE_FLOAT32) then
            dtypeobj = me%NumPy%NUMPY_DTYPE_FLOAT32
            typebytes = 4
        elseif (arrtype == FYPY_NUMPY_DTYPE_FLOAT64) then
            dtypeobj = me%NumPy%NUMPY_DTYPE_FLOAT64
            typebytes = 8
        elseif (arrtype == FYPY_NUMPY_DTYPE_Complex128) then
            dtypeobj = me%NumPy%NUMPY_DTYPE_COMPLEX128
            typebytes = 16
        else
            dtypeobj = C_NULL_PTR
            Fy2PyArray = FYPY_ERROR_NUMPY_ARRAY_DTYPE
            typebytes = -1
            return
        end if

        ! set Fortran-style ordering
        status = Fy2PyType('F', orderobj)
        
        ! create arguments for ndarray call
        kwargs = PyDict_New()
        error = PyDict_SetItemString(kwargs, 'shape'//C_NULL_CHAR, shapeobj)
        error = PyDict_SetItemString(kwargs, 'dtype'//C_NULL_CHAR, dtypeobj)
        error = PyDict_SetItemString(kwargs, 'order'//C_NULL_CHAR, orderobj)
        
        if (status == FyPy_SUCCESS .AND. error == 0) then
            ! Create the array of the required shape, size and data type
            call FyPyCallPyObject(me%NumPy%ndarray, pyarr, kwargs=kwargs)

            if (c_associated(pyarr)) then
                ! get the buffer for copying raw data
                flags = ior(PyBUF_WRITABLE, PyBUF_F_CONTIGUOUS)
                error = PyObject_GetBuffer(pyarr, c_loc(PyBuffer), flags)
                
                if (error /= 0) then
                    status = FYPY_ERROR_NUMPY_BUFFER_ERROR
                else
                    ! check if buffer properties match?
                    if (PyBuffer%ndim /= arrrank .OR. &
                            PyBuffer%length /= typebytes*product(arrshape)) then
                        status = FYPY_ERROR_NUMPY_BUFFER_FORMAT
                    end if
                end if
            else
                status = FYPY_ERROR_PYCALL
            end if
        end if

        ! return if error
        if (status /= FYPY_SUCCESS) then
            call Py_XDecref(Kwargs)
            call Py_XDecref(orderobj)
            call PyBuffer_Release(c_loc(PyBuffer))   
            Fy2PyArray = status
            return
        end if
        
       ! Define and use the right pointer to copy the given data
       select case (arrtype)
       
       case (FYPY_NUMPY_DTYPE_INT8)
           if (arrrank == 1) then
               call c_f_pointer(arr,pi8r1data,arrshape)
               call c_f_pointer(PyBuffer%buf,pi8r1arr,arrshape)
               pi8r1arr = pi8r1data
           elseif (arrrank == 2) then
                call c_f_pointer(arr,pi8r2data,arrshape)
                call c_f_pointer(PyBuffer%buf,pi8r2arr,arrshape)
                pi8r2arr = pi8r2data
           elseif (arrrank == 3) then
                call c_f_pointer(arr,pi8r3data,arrshape)
                call c_f_pointer(PyBuffer%buf,pi8r3arr,arrshape)
                pi8r3arr = pi8r3data
           elseif (arrrank == 4) then
                call c_f_pointer(arr,pi8r4data,arrshape)
                call c_f_pointer(PyBuffer%buf,pi8r4arr,arrshape)
                pi8r4arr = pi8r4data
           else
               status = FYPY_ERROR_NUMPY_ARRAY_SHAPE
           end if
       case (FYPY_NUMPY_DTYPE_INT32)
           if (arrrank == 1) then
                call c_f_pointer(arr,pi32r1data,arrshape)
                call c_f_pointer(PyBuffer%buf,pi32r1arr,arrshape)
                pi32r1arr = pi32r1data
           elseif (arrrank == 2) then
                call c_f_pointer(arr,pi32r2data,arrshape)                
                call c_f_pointer(PyBuffer%buf,pi32r2arr,arrshape)
                pi32r2arr = pi32r2data
           elseif (arrrank == 3) then
                call c_f_pointer(arr,pi32r3data,arrshape)
                call c_f_pointer(PyBuffer%buf,pi32r3arr,arrshape)
                pi32r3arr = pi32r3data
           elseif (arrrank == 4) then
                call c_f_pointer(arr,pi32r4data,arrshape)
                call c_f_pointer(PyBuffer%buf,pi32r4arr,arrshape)
                pi32r4arr = pi32r4data
           else
               status = FYPY_ERROR_NUMPY_ARRAY_SHAPE
           end if
       case (FYPY_NUMPY_DTYPE_INT64)
           if (arrrank == 1) then
                call c_f_pointer(arr,pi64r1data,arrshape)
                call c_f_pointer(PyBuffer%buf,pi64r1arr,arrshape)
                pi64r1arr = pi64r1data
           elseif (arrrank == 2) then
                call c_f_pointer(arr,pi64r2data,arrshape)
                call c_f_pointer(PyBuffer%buf,pi64r2arr,arrshape)
                pi64r2arr = pi64r2data
           elseif (arrrank == 3) then
                call c_f_pointer(arr,pi64r3data,arrshape)
                call c_f_pointer(PyBuffer%buf,pi64r3arr,arrshape)
                pi64r3arr = pi64r3data
           elseif (arrrank == 4) then
                call c_f_pointer(arr,pi64r4data,arrshape)
                call c_f_pointer(PyBuffer%buf,pi64r4arr,arrshape)
                pi64r4arr = pi64r4data
           else
               status = FYPY_ERROR_NUMPY_ARRAY_SHAPE
           end if            
       case (FYPY_NUMPY_DTYPE_FLOAT32)
           if (arrrank == 1) then
                call c_f_pointer(arr,pr32r1data,arrshape)
                call c_f_pointer(PyBuffer%buf,pr32r1arr,arrshape)
                pr32r1arr = pr32r1data
           elseif (arrrank == 2) then
                call c_f_pointer(arr,pr32r2data,arrshape)
                call c_f_pointer(PyBuffer%buf,pr32r2arr,arrshape)
                pr32r2arr = pr32r2data
           elseif (arrrank == 3) then
                call c_f_pointer(arr,pr32r3data,arrshape)
                call c_f_pointer(PyBuffer%buf,pr32r3arr,arrshape)
                pr32r3arr = pr32r3data
           elseif (arrrank == 4) then
                call c_f_pointer(arr,pr32r4data,arrshape)
                call c_f_pointer(PyBuffer%buf,pr32r4arr,arrshape)
                pr32r4arr = pr32r4data
           else
               status = FYPY_ERROR_NUMPY_ARRAY_SHAPE
           end if                        
       case (FYPY_NUMPY_DTYPE_FLOAT64)
           if (arrrank == 1) then
                call c_f_pointer(arr,pr64r1data,arrshape)
                call c_f_pointer(PyBuffer%buf,pr64r1arr,arrshape)
                pr64r1arr = pr64r1data
           elseif (arrrank == 2) then
                call c_f_pointer(arr,pr64r2data,arrshape)
                call c_f_pointer(PyBuffer%buf,pr64r2arr,arrshape)
                pr64r2arr = pr64r2data
           elseif (arrrank == 3) then
                call c_f_pointer(arr,pr64r3data,arrshape)
                call c_f_pointer(PyBuffer%buf,pr64r3arr,arrshape)
                pr64r3arr = pr64r3data
           elseif (arrrank == 4) then
                call c_f_pointer(arr,pr64r4data,arrshape)
                call c_f_pointer(PyBuffer%buf,pr64r4arr,arrshape)
                pr64r4arr = pr64r4data
           else
               status = FYPY_ERROR_NUMPY_ARRAY_SHAPE
           end if                                    
       case (FYPY_NUMPY_DTYPE_Complex128)
           if (arrrank == 1) then
                call c_f_pointer(arr,pc64r1data,arrshape)
                call c_f_pointer(PyBuffer%buf,pc64r1arr,arrshape)
                pc64r1arr = pc64r1data
           elseif (arrrank == 2) then
                call c_f_pointer(arr,pc64r2data,arrshape)
                call c_f_pointer(PyBuffer%buf,pc64r2arr,arrshape)
                pc64r2arr = pc64r2data
           elseif (arrrank == 3) then
                call c_f_pointer(arr,pc64r3data,arrshape)
                call c_f_pointer(PyBuffer%buf,pc64r3arr,arrshape)
                pc64r3arr = pc64r3data
           elseif (arrrank == 4) then
                call c_f_pointer(arr,pc64r4data,arrshape)
                call c_f_pointer(PyBuffer%buf,pc64r4arr,arrshape)
                pc64r4arr = pc64r4data
           else
               status = FYPY_ERROR_NUMPY_ARRAY_SHAPE
           end if                                    
       case default
           status = FYPY_ERROR_NUMPY_ARRAY_DTYPE
       end select 
    
        ! free memory
        call Py_XDecref(Kwargs)
        call Py_XDecref(orderobj)
        call PyBuffer_Release(c_loc(PyBuffer))   
        if (status /= FYPY_SUCCESS) then
            Fy2PyArray = status
            call Py_XDecref(pyarr)
        end if

    end function Fy2PyArray
    



    integer(kind(FYPY_SUCCESS)) module function Py2FyArray(me, pyarr, arrtype, arrshape, arr)
        implicit none
        class(FyPyclass), intent(inout) :: me
        type(C_PTR), intent(in) :: pyarr
        integer(kind(FYPY_NUMPY_DTYPE_INT8)), intent(out) :: arrtype
        integer, dimension(:), allocatable, intent(out) :: arrshape
        type(C_PTR), intent(out) :: arr

        ! fortran pointers for int8 data
        integer(C_INT8_T), dimension(:), pointer :: pi8r1data,pi8r1arr
        integer(C_INT8_T), dimension(:,:), pointer :: pi8r2data,pi8r2arr
        integer(C_INT8_T), dimension(:,:,:), pointer :: pi8r3data,pi8r3arr
        integer(C_INT8_T), dimension(:,:,:,:), pointer :: pi8r4data,pi8r4arr

       ! fortran pointers for int32 data
        integer(C_INT32_T), dimension(:), pointer :: pi32r1data,pi32r1arr
        integer(C_INT32_T), dimension(:,:), pointer :: pi32r2data,pi32r2arr
        integer(C_INT32_T), dimension(:,:,:), pointer :: pi32r3data,pi32r3arr
        integer(C_INT32_T), dimension(:,:,:,:), pointer :: pi32r4data,pi32r4arr

        ! fortran pointers for int64 data
        integer(C_INT64_T), dimension(:), pointer :: pi64r1data,pi64r1arr
        integer(C_INT64_T), dimension(:,:), pointer :: pi64r2data,pi64r2arr
        integer(C_INT64_T), dimension(:,:,:), pointer :: pi64r3data,pi64r3arr
        integer(C_INT64_T), dimension(:,:,:,:), pointer :: pi64r4data,pi64r4arr

        ! fortran pointers for real32 data
        real(C_FLOAT), dimension(:), pointer :: pr32r1data,pr32r1arr
        real(C_FLOAT), dimension(:,:), pointer :: pr32r2data,pr32r2arr
        real(C_FLOAT), dimension(:,:,:), pointer :: pr32r3data,pr32r3arr
        real(C_FLOAT), dimension(:,:,:,:), pointer :: pr32r4data,pr32r4arr

        ! fortran pointers for real64 data
        real(C_DOUBLE), dimension(:), pointer :: pr64r1data,pr64r1arr
        real(C_DOUBLE), dimension(:,:), pointer :: pr64r2data,pr64r2arr
        real(C_DOUBLE), dimension(:,:,:), pointer :: pr64r3data,pr64r3arr
        real(C_DOUBLE), dimension(:,:,:,:), pointer :: pr64r4data,pr64r4arr

        ! fortran pointers for complex64 data
        complex(C_DOUBLE_COMPLEX), dimension(:), pointer :: pc64r1data,pc64r1arr
        complex(C_DOUBLE_COMPLEX), dimension(:,:), pointer :: pc64r2data,pc64r2arr
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:), pointer :: pc64r3data,pc64r3arr
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:,:), pointer :: pc64r4data,pc64r4arr

        integer(kind(FYPY_SUCCESS))     :: status
        type(Py_Buffer)                 :: PyBuffer
        type(C_PTR)                     :: kwargs, obj, obj1, ret, shapeobj, dtypeobj, orderobj
        integer(C_INT)                  :: error, flags, order
        integer(PY_SSIZE_T)             :: ctr, arrrank
        character(len=:), allocatable   :: dtypestr

        nullify(pi8r1data,pi8r1arr,pi8r2data,pi8r2arr,pi8r3data,pi8r3arr,pi8r4data,pi8r4arr)
        nullify(pi32r1data,pi32r1arr,pi32r2data,pi32r2arr,pi32r3data,pi32r3arr,pi32r4data,pi32r4arr)
        nullify(pi64r1data,pi64r1arr,pi64r2data,pi64r2arr,pi64r3data,pi64r3arr,pi64r4data,pi64r4arr)
        nullify(pr32r1data,pr32r1arr,pr32r2data,pr32r2arr,pr32r3data,pr32r3arr,pr32r4data,pr32r4arr)
        nullify(pr64r1data,pr64r1arr,pr64r2data,pr64r2arr,pr64r3data,pr64r3arr,pr64r4data,pr64r4arr)
        nullify(pc64r1data,pc64r1arr,pc64r2data,pc64r2arr,pc64r3data,pc64r3arr,pc64r4data,pc64r4arr)

        ! reset
        Py2FyArray = FYPY_SUCCESS
        arr = C_NULL_PTR

        ! get array shape
        shapeobj = PyObject_GetAttrString(pyarr, 'shape'//C_NULL_CHAR)
        arrrank = PyTuple_Size(shapeobj)
        allocate(arrshape(arrrank))
        do ctr = 1, arrrank
            obj = PyTuple_GetItem(shapeobj, ctr-1)
            if (c_associated(obj)) then
                status = Py2FyType(obj, arrshape(ctr))
                if (status /= FYPY_SUCCESS) then
                    Py2FyArray = FYPY_ERROR_NUMPY_ARRAY_SHAPE
                    exit
                end if
            end if
        end do
        

        call Py_XDecref(shapeobj)

        if (Py2FyArray /= FYPY_SUCCESS) return
        
        ! get data type
        obj = PyObject_GetAttrString(pyarr, 'dtype'//C_NULL_CHAR)
        
        if (c_associated(obj)) then
            
            obj1 = PyTuple_New(1_PY_SSIZE_T)
            error = PyTuple_SetItem(obj1,0_PY_SSIZE_T,obj)
            
            call FyPyCallPyObject(me%str, ret, obj1)
            
            if (c_associated(ret)) then
                
                status = Py2FyType(ret, dtypestr)  
            
                if (dtypestr == 'int8') then
                    arrtype = FYPY_NUMPY_DTYPE_INT8
                elseif (dtypestr == 'int32') then
                    arrtype = FYPY_NUMPY_DTYPE_INT32
                elseif (dtypestr == 'int64') then
                    arrtype = FYPY_NUMPY_DTYPE_INT64
                elseif (dtypestr == 'float32') then
                    arrtype = FYPY_NUMPY_DTYPE_FLOAT32
                elseif (dtypestr == 'float64') then
                    arrtype = FYPY_NUMPY_DTYPE_FLOAT64
                elseif (dtypestr == 'complex128') then
                    arrtype = FYPY_NUMPY_DTYPE_Complex128
                else
                    arrtype = FYPY_NUMPY_DTYPE_UNKNOWN
                    Py2FyArray = FYPY_ERROR_NUMPY_ARRAY_DTYPE
                end if
            else
                Py2FyArray = FYPY_ERROR_PYCALL
            end if
        end if
        
        
        call Py_XDecref(obj)
        call Py_XDecref(obj1)
        if (Py2FyArray /= FYPY_SUCCESS) return
        
        ! Now get the buffer object from the python array
        flags = ior(PyBUF_FORMAT, PyBUF_F_CONTIGUOUS)
        error = PyObject_GetBuffer(pyarr, c_loc(PyBuffer), flags)
        
        if (error /= 0) then
            Py2FyArray = FYPY_ERROR_NUMPY_BUFFER_ERROR
        else
            ! check whether C-style or Fortran-style ordering
            order = PyBuffer_IsContiguous(c_loc(PyBuffer), 'F')
            if (order /= 1) then
                Py2FyArray = FYPY_ERROR_NUMPY_NOT_CONTIGUOUS
            end if      
        end if
        
        if (Py2FyArray /= FYPY_SUCCESS) then
            call PyBuffer_Release(c_loc(PyBuffer))   
            return
        end if

        ! we are here, that means we are ready to copy the data

        ! Define and use the right pointer to copy the given data
        select case (arrtype)
        
        case (FYPY_NUMPY_DTYPE_INT8)
            if (arrrank == 1) then
                 allocate(pi8r1arr(arrshape(1)))
                 call c_f_pointer(PyBuffer%buf,pi8r1data,arrshape)
                 pi8r1arr = pi8r1data         
                 arr = c_loc(pi8r1arr)
           elseif (arrrank == 2) then
                allocate(pi8r2arr(arrshape(1),arrshape(2)))
                call c_f_pointer(PyBuffer%buf,pi8r2data,arrshape)
                pi8r2arr = pi8r2data
                arr = c_loc(pi8r2arr)
           elseif (arrrank == 3) then
                allocate(pi8r3arr(arrshape(1),arrshape(2),arrshape(3)))
                call c_f_pointer(PyBuffer%buf,pi8r3data,arrshape)
                pi8r3arr = pi8r3data
                arr = c_loc(pi8r3arr)
           elseif (arrrank == 4) then
                allocate(pi8r4arr(arrshape(1),arrshape(2),arrshape(3),arrshape(4)))
                call c_f_pointer(PyBuffer%buf,pi8r4data,arrshape)
                pi8r4arr = pi8r4data
                arr = c_loc(pi8r4arr)
           else
               status = FYPY_ERROR_NUMPY_ARRAY_SHAPE
           end if
        case (FYPY_NUMPY_DTYPE_INT32)
            if (arrrank == 1) then
                allocate(pi32r1arr(arrshape(1)))
                call c_f_pointer(PyBuffer%buf,pi32r1data,arrshape)
                pi32r1arr = pi32r1data
                arr = c_loc(pi32r1arr)
            elseif (arrrank == 2) then
                allocate(pi32r2arr(arrshape(1),arrshape(2)))
                call c_f_pointer(PyBuffer%buf,pi32r2data,arrshape)
                pi32r2arr = pi32r2data
                arr = c_loc(pi32r2arr)
            elseif (arrrank == 3) then
                allocate(pi32r3arr(arrshape(1),arrshape(2),arrshape(3)))
                call c_f_pointer(PyBuffer%buf,pi32r3data,arrshape)
                pi32r3arr = pi32r3data
                arr = c_loc(pi32r3arr)
            elseif (arrrank == 4) then
                allocate(pi32r4arr(arrshape(1),arrshape(2),arrshape(3),arrshape(4)))
                call c_f_pointer(PyBuffer%buf,pi32r4data,arrshape)
                pi32r4arr = pi32r4data
                arr = c_loc(pi32r4arr)
            else
                status = FYPY_ERROR_NUMPY_ARRAY_SHAPE
            end if
        case (FYPY_NUMPY_DTYPE_INT64)
            if (arrrank == 1) then
                allocate(pi64r1arr(arrshape(1)))
                call c_f_pointer(PyBuffer%buf,pi64r1data,arrshape)
                pi64r1arr = pi64r1data
                arr = c_loc(pi64r1arr)
            elseif (arrrank == 2) then
               allocate(pi64r2arr(arrshape(1),arrshape(2)))
                call c_f_pointer(PyBuffer%buf,pi64r2data,arrshape)
                pi64r2arr = pi64r2data
                arr = c_loc(pi64r2arr)
            elseif (arrrank == 3) then
                allocate(pi64r3arr(arrshape(1),arrshape(2),arrshape(3)))
                call c_f_pointer(PyBuffer%buf,pi64r3data,arrshape)
                pi64r3arr = pi64r3data
                arr = c_loc(pi64r3arr)
            elseif (arrrank == 4) then
                allocate(pi64r4arr(arrshape(1),arrshape(2),arrshape(3),arrshape(4)))
                call c_f_pointer(PyBuffer%buf,pi64r4data,arrshape)
                pi64r4arr = pi64r4data
                arr = c_loc(pi64r4arr)
            else
               status = FYPY_ERROR_NUMPY_ARRAY_SHAPE
            end if            
        
        case (FYPY_NUMPY_DTYPE_FLOAT32)
            if (arrrank == 1) then
                allocate(pr32r1arr(arrshape(1)))
                call c_f_pointer(PyBuffer%buf,pr32r1data,arrshape)
                pr32r1arr = pr32r1data
                arr = c_loc(pr32r1arr)
            elseif (arrrank == 2) then
               allocate(pr32r2arr(arrshape(1),arrshape(2)))
                call c_f_pointer(PyBuffer%buf,pr32r2data,arrshape)
                pr32r2arr = pr32r2data
                arr = c_loc(pr32r2arr)
            elseif (arrrank == 3) then
                allocate(pr32r3arr(arrshape(1),arrshape(2),arrshape(3)))
                call c_f_pointer(PyBuffer%buf,pr32r3data,arrshape)
                pr32r3arr = pr32r3data
                arr = c_loc(pr32r3arr)
            elseif (arrrank == 4) then
                allocate(pr32r4arr(arrshape(1),arrshape(2),arrshape(3),arrshape(4)))
                call c_f_pointer(PyBuffer%buf,pr32r4data,arrshape)
                pr32r4arr = pr32r4data
                arr = c_loc(pr32r4arr)
            else
                status = FYPY_ERROR_NUMPY_ARRAY_SHAPE
            end if                                    
        
        case (FYPY_NUMPY_DTYPE_FLOAT64)
            if (arrrank == 1) then
                allocate(pr64r1arr(arrshape(1)))
                call c_f_pointer(PyBuffer%buf,pr64r1data,arrshape)
                pr64r1arr = pr64r1data
                arr = c_loc(pr64r1arr)
            elseif (arrrank == 2) then
                allocate(pr64r2arr(arrshape(1),arrshape(2)))
                call c_f_pointer(PyBuffer%buf,pr64r2data,arrshape)
                pr64r2arr = pr64r2data
                arr = c_loc(pr64r2arr)
            elseif (arrrank == 3) then
                allocate(pr64r3arr(arrshape(1),arrshape(2),arrshape(3)))
                call c_f_pointer(PyBuffer%buf,pr64r3data,arrshape)
                pr64r3arr = pr64r3data
                arr = c_loc(pr64r3arr)
            elseif (arrrank == 4) then
                allocate(pr64r4arr(arrshape(1),arrshape(2),arrshape(3),arrshape(4)))
                call c_f_pointer(PyBuffer%buf,pr64r4data,arrshape)
                pr64r4arr = pr64r4data
                arr = c_loc(pr64r4arr)
            else
               status = FYPY_ERROR_NUMPY_ARRAY_SHAPE
           end if                        
        
        case (FYPY_NUMPY_DTYPE_Complex128)
            if (arrrank == 1) then
                allocate(pc64r1arr(arrshape(1)))
                call c_f_pointer(PyBuffer%buf,pc64r1data,arrshape)
                pc64r1arr = pc64r1data
                arr = c_loc(pc64r1arr)
            elseif (arrrank == 2) then
               allocate(pc64r2arr(arrshape(1),arrshape(2)))
                call c_f_pointer(PyBuffer%buf,pc64r2data,arrshape)
                pc64r2arr = pc64r2data
                arr = c_loc(pc64r2arr)
            elseif (arrrank == 3) then
                allocate(pc64r3arr(arrshape(1),arrshape(2),arrshape(3)))
                call c_f_pointer(PyBuffer%buf,pc64r3data,arrshape)
                pc64r3arr = pc64r3data
                arr = c_loc(pc64r3arr)
            elseif (arrrank == 4) then
                allocate(pc64r4arr(arrshape(1),arrshape(2),arrshape(3),arrshape(4)))
                call c_f_pointer(PyBuffer%buf,pc64r4data,arrshape)
                pc64r4arr = pc64r4data
                arr = c_loc(pc64r4arr)
            else
                status = FYPY_ERROR_NUMPY_ARRAY_SHAPE
            end if                                    
        case default
            status = FYPY_ERROR_NUMPY_ARRAY_DTYPE
        end select 
        
        ! free memory
        call PyBuffer_Release(c_loc(PyBuffer))   
        Py2FyArray = status

    end function Py2FyArray





end submodule FyPyTypes


