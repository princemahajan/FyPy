!############################################################################################
!
!  TestFyPy: Test Module for FyPy
!
!> \brief       TestFyPy module
!! \details     Test script for FyPy
!! \author      Bharat Mahajan
!! \date        08/13/2019    
!
!############################################################################################



module TestFyPy

    use, intrinsic :: iso_fortran_env
    use FyPy

    implicit none

    type(FyPyClass), save :: fypyobj

    character(kind=C_CHAR,len=*), parameter :: mname = 'test'
    character(kind=C_CHAR,len=*), parameter :: mdocstr = 'This is test method of FyPy'

    character(kind=C_CHAR,len=*), parameter :: modname = 'TestFyPy'
    character(kind=C_CHAR,len=*), parameter :: moddoc = 'Test Module for FyPy Library'


    integer(int32) :: l1
    integer(int64) :: ll1
    real(real64) :: r1
    logical :: b1
    complex(real64) :: c1


    ! call back function pointer
    type(C_PTR), save :: callback = C_NULL_PTR


    contains

    type(C_PTR) function PyInit_TestFyPy() bind(C, name="PyInit_TestFyPy")

        !dir$ ATTRIBUTES DLLEXPORT :: PyInit_TestFyPy

        implicit none

        type(C_PTR) :: obj, obj1, modobj
        integer(kind(FYPY_SUCCESS)) :: status 
        character(len=:), allocatable :: bstr
        integer(C_INT8_T), dimension(1) :: i8arr
        integer(C_INT32_T), dimension(2,2) :: i32arr
        integer(C_INT64_T), dimension(3,3,3) :: i64arr
        real(C_FLOAT), dimension(2,2,2,2) :: r32arr
        real(C_DOUBLE), dimension(3,5) :: r64arr
        
        integer(C_INT8_T), dimension(:),pointer :: i8arr1
        integer(C_INT32_T),  dimension(:,:),pointer :: i32arr1
        integer(C_INT64_T), dimension(:,:,:),pointer :: i64arr1
        
        real(C_FLOAT),dimension(:,:,:,:),pointer :: r32arr1
        real(C_DOUBLE),  dimension(:,:),pointer :: r64arr1
        integer(kind(FYPY_NUMPY_DTYPE_INT8)) :: arrtype
        integer, dimension(:), allocatable :: arrshape
        
        
        integer :: ctr
!        procedure(PyCFunction), pointer :: pTestFyPyMethod

        l1 = 123
        ll1 = 987654321
        r1 = 12.345678901
        b1 = .FALSE.
        c1 = cmplx(987.654321,12.3456789)

        PyInit_TestFyPy = C_NULL_PTR
        
        call fypyobj%Init(1,3,.TRUE.)
        
        if (fypyobj%status /= FYPY_SUCCESS) return
        
        call fypyobj%AddMethod(mname,mdocstr,ArgsMethod=TestFyPyMethod)
        call fypyobj%AddMethod('testparse','test of parse',ArgsMethod=TestFyPyParseMethod)
        call fypyobj%AddMethod('testkw','test of kw callable',ArgsKwMethod=TestFyPyDictMethod)
        call fypyobj%AddMethod('testcb','test of python callback',ArgsMethod=TestFyPyCallback)
        modobj = fypyobj%CreatePyModule(modname,moddoc)
        

        ! string test
        status = Fy2PyType('pi', obj)
        status = Py2FyType(obj, bstr)
        status = Fy2PyType(3.14159_C_DOUBLE, obj)
        call fypyobj%AddObject(modobj, bstr, obj)

        ! long test
        status = Fy2PyType(l1, obj1)
        status = Py2FyType(obj1, l1)
        status = Fy2PyType(l1, obj1)
        call fypyobj%AddObject(modobj, 'l1', obj1)

        ! long long test
        status = Fy2PyType(ll1, obj1)
        status = Py2FyType(obj1, ll1)
        status = Fy2PyType(ll1, obj1)
        call fypyobj%AddObject(modobj, 'll1', obj1)

        ! real test
        status = Fy2PyType(r1, obj1)
        status = Py2FyType(obj1, r1)
        status = Fy2PyType(r1, obj1)
        call fypyobj%AddObject(modobj, 'r1', obj1)

        ! bool test
        status = Fy2PyType(b1, obj1)
        status = Py2FyType(obj1, b1)
        status = Fy2PyType(b1, obj1)
        call fypyobj%AddObject(modobj, 'b1', obj1)

        ! long test
        status = Fy2PyType(c1, obj1)
        status = Py2FyType(obj1, c1)
        status = Fy2PyType(c1, obj1)
        call fypyobj%AddObject(modobj, 'c1', obj1)

        ! ndarray create test
        i8arr = [100]
        status = fypyobj%Fy2PyArray(c_loc(i8arr), FYPY_NUMPY_DTYPE_INT8, shape(i8arr), obj1)
        if (c_associated(obj1)) then
            call fypyobj%AddObject(modobj, 'i8arr', obj1)
        else
            block
            character(len=100) :: errstr
            write(errstr,*) 'err=', status
            call FyPyPrint(errstr)
            end block        
        end if
        status = fypyobj%Py2FyArray(obj1,arrtype,arrshape, obj)
        if (c_associated(obj)) then
            call c_f_pointer(obj,i8arr1,arrshape)
            block
                character(len=400) :: astr
                write(Astr,*) 'arr=', i8arr1
                call FyPyPrint(astr)
            end block                    
        else
            block
            character(len=100) :: errstr
            write(errstr,*) 'err=', status
            call FyPyPrint(errstr)
            end block        
        end if
        
        
        
        i32arr = reshape([1,2,3,4],[2,2])
        status = fypyobj%Fy2PyArray(c_loc(i32arr), FYPY_NUMPY_DTYPE_INT32, shape(i32arr), obj1)
        
        if (c_associated(obj1)) then
            call fypyobj%AddObject(modobj, 'i32arr', obj1)
        else
            block
            character(len=100) :: errstr
            write(errstr,*) 'err=', status
            call FyPyPrint(errstr)
            end block        
        end if
        status = fypyobj%Py2FyArray(obj1,arrtype,arrshape, obj)
        if (c_associated(obj)) then
            call c_f_pointer(obj,i32arr1,arrshape)
            block
                character(len=2000) :: astr
                write(Astr,*) 'arr=', i32arr1
                call FyPyPrint(astr)
            end block                    
        else
            block
            character(len=100) :: errstr
            write(errstr,*) 'err=', status
            call FyPyPrint(errstr)
            end block        
        end if
        
        i64arr = reshape([(ctr,ctr=1,product(shape(i64arr)))],shape(i64arr))
        status = fypyobj%Fy2PyArray(c_loc(i64arr), FYPY_NUMPY_DTYPE_INT64, shape(i64arr), obj1)
        if (c_associated(obj1)) then
            call fypyobj%AddObject(modobj, 'i64arr', obj1)
        else
            block
            character(len=100) :: errstr
            write(errstr,*) 'err=', status
            call FyPyPrint(errstr)
            end block        
        end if
        status = fypyobj%Py2FyArray(obj1,arrtype,arrshape, obj)
        if (c_associated(obj)) then
            call c_f_pointer(obj,i64arr1,arrshape)
            block
                character(len=2000) :: astr
                write(Astr,*) 'arr=', i64arr1
                call FyPyPrint(astr)
            end block                    
        else
            block
            character(len=100) :: errstr
            write(errstr,*) 'err=', status
            call FyPyPrint(errstr)
            end block        
        end if
        
        
        r32arr = reshape([(real(ctr,kind=C_FLOAT),ctr=1,product(shape(r32arr)))],shape(r32arr))
        status = fypyobj%Fy2PyArray(c_loc(r32arr), FYPY_NUMPY_DTYPE_FLOAT32, shape(r32arr), obj1)
        if (c_associated(obj1)) then
            call fypyobj%AddObject(modobj, 'r32arr', obj1)
        else
            block
            character(len=100) :: errstr
            write(errstr,*) 'err=', status
            call FyPyPrint(errstr)
            end block        
        end if
        status = fypyobj%Py2FyArray(obj1,arrtype,arrshape, obj)
        if (c_associated(obj)) then
            call c_f_pointer(obj,r32arr1,arrshape)
            block
                character(len=6000) :: astr
                write(Astr,*) 'arr=', r32arr1
                call FyPyPrint(astr)
            end block                    
        else
            block
            character(len=100) :: errstr
            write(errstr,*) 'err=', status
            call FyPyPrint(errstr)
            end block        
        end if
        
        r64arr = reshape([(real(ctr,kind=C_DOUBLE),ctr=1,product(shape(r64arr)))],shape(r64arr))
        status = fypyobj%Fy2PyArray(c_loc(r64arr), FYPY_NUMPY_DTYPE_FLOAT64, shape(r64arr), obj1)
        if (c_associated(obj1)) then
            call fypyobj%AddObject(modobj, 'r64arr', obj1)
        else
            block
            character(len=100) :: errstr
            write(errstr,*) 'err=', status
            call FyPyPrint(errstr)
            end block        
        end if
        status = fypyobj%Py2FyArray(obj1,arrtype,arrshape, obj)
        if (c_associated(obj)) then
            call c_f_pointer(obj,r64arr1,arrshape)
            block
                character(len=2000) :: astr
                write(Astr,*) 'arr=', r64arr1
                call FyPyPrint(astr)
            end block                    
        else
            block
            character(len=100) :: errstr
            write(errstr,*) 'err=', status
            call FyPyPrint(errstr)
            end block        
        end if
        
        ! return
        PyInit_TestFyPy = modobj

    end function PyInit_TestFyPy




    function TestFyPyMethod(self, args) result(ret) bind(C)
        implicit none
        type(C_PTR), value :: self
        type(C_PTR), value    :: args
        type(C_PTR)           :: ret

        integer(C_LONG) :: tmpint, err
        integer(PY_SSIZE_T) :: sz

        integer(kind(FYPY_SUCCESS)) :: status
        type(C_PTR) :: intuple, inarg, outtuple

        integer(C_LONG) :: l1
        integer(C_LONG_LONG) :: ll1
        real(C_DOUBLE) :: r1
        logical :: b1
        complex(C_DOUBLE) :: c1

        ! check for tuple type
        intuple = args
        if (IsPyTupleType(intuple, .TRUE.)) then

            ! create out tuple 
            sz = PyTuple_Size(intuple)
            outtuple = PyTuple_New(sz)
    
            if (sz == 5) then
            ! extract the tuple objects

            ! long integer
            inarg = PyTuple_GetItem(intuple,0_PY_SSIZE_T)
            status = Py2FyType(inarg, l1)
            status = Fy2PyType(l1+1, inarg)
            err = PyTuple_SetItem(outtuple, 0_PY_SSIZE_T, inarg)            

            ! long long integer
            inarg = PyTuple_GetItem(intuple,1_PY_SSIZE_T)
            status = Py2FyType(inarg, ll1)
            status = Fy2PyType(ll1+999999999, inarg)
            err = PyTuple_SetItem(outtuple, 1_PY_SSIZE_T, inarg)            

            !  real
            inarg = PyTuple_GetItem(intuple,2_PY_SSIZE_T)
            status = Py2FyType(inarg, r1)
            status = Fy2PyType(r1+1.5, inarg)
            err = PyTuple_SetItem(outtuple, 2_PY_SSIZE_T, inarg)            

            !  boolean
            inarg = PyTuple_GetItem(intuple,3_PY_SSIZE_T)
            status = Py2FyType(inarg, b1)
            status = Fy2PyType((.NOT. b1), inarg)
            err = PyTuple_SetItem(outtuple, 3_PY_SSIZE_T, inarg)            

            ! complex
            inarg = PyTuple_GetItem(intuple,4_PY_SSIZE_T)
            status = Py2FyType(inarg, c1)
            status = Fy2PyType(c1+cmplx(1,1,kind=C_DOUBLE), inarg)
            err = PyTuple_SetItem(outtuple, 4_PY_SSIZE_T, inarg)    
            
            else
                ! error message
                status = Fy2PyType('Need 5 parameters in sequence (long, long long, float, bool, complex', inarg)
                err = PyTuple_SetItem(outtuple, 0_PY_SSIZE_T, inarg)    
            end if

            ret = outtuple
        else
            status = Fy2PyType('Extraction failed', ret)
        end if
        

    end function TestFyPyMethod



    function TestFyPyParseMethod(self, args) result(ret) bind(C)
        implicit none
        type(C_PTR), value :: self
        type(C_PTR), value    :: args
        type(C_PTR)           :: ret

        integer(C_LONG) :: tmpint, err
        integer(PY_SSIZE_T) :: sz

        integer(kind(FYPY_SUCCESS)) :: status
        type(C_PTR) :: intuple, inarg, outtuple

        integer(C_LONG) :: l1
        integer(C_LONG_LONG) :: ll1
        real(C_DOUBLE) :: r1
        logical :: b1
        complex(C_DOUBLE) :: c1

        integer(C_INT8_T), dimension(:), pointer :: i8arr
        integer(C_INT32_T), dimension(:,:), pointer :: i32arr
        integer(C_INT64_T), dimension(:,:,:), pointer :: i64arr
        real(C_FLOAT), dimension(:,:,:,:), pointer :: r32arr
        real(C_DOUBLE), dimension(:,:), pointer :: r64arr

        ! input tuple parse format
        type(FyPyMethodArgsFormat), dimension(10) :: InpArgFmt

        ! Input Fortran args list
        type(List) :: InArgsList

        class(*), allocatable :: item
        integer :: listctr

        ! check for tuple type
        intuple = args
        if (IsPyTupleType(intuple, .TRUE.)) then

            ! create out tuple 
            sz = PyTuple_Size(intuple)
            outtuple = PyTuple_New(sz)
    
            ! Input arguments format

            InpArgFmt(1)%PyObjType = FYPY_LONG
            InpArgFmt(1)%optional = .FALSE.

            InpArgFmt(2)%PyObjType = FYPY_LONG_LONG_INT
            InpArgFmt(2)%optional = .FALSE.

            InpArgFmt(3)%PyObjType = FYPY_DOUBLE
            InpArgFmt(3)%optional = .FALSE.

            InpArgFmt(4)%PyObjType = FYPY_BOOL
            InpArgFmt(4)%optional = .FALSE.

            InpArgFmt(5)%PyObjType = FYPY_COMPLEX_DOUBLE
            InpArgFmt(5)%optional = .FALSE.

            InpArgFmt(6)%IsNumpyArr = .TRUE.
            InpArgFmt(6)%NumpyDType = FYPY_NUMPY_DTYPE_INT8
            InpArgFmt(6)%NumpyArrRank = 1
            InpArgFmt(6)%NumpyArrExtent = [1]
            InpArgFmt(6)%optional = .FALSE.

            InpArgFmt(7)%IsNumpyArr = .TRUE.
            InpArgFmt(7)%NumpyDType = FYPY_NUMPY_DTYPE_INT32
            InpArgFmt(7)%NumpyArrRank = 2
            InpArgFmt(7)%NumpyArrExtent = [2,2]
            InpArgFmt(7)%optional = .FALSE.

            InpArgFmt(8)%IsNumpyArr = .TRUE.
            InpArgFmt(8)%NumpyDType = FYPY_NUMPY_DTYPE_INT64
            InpArgFmt(8)%NumpyArrRank = 3
            InpArgFmt(8)%NumpyArrExtent = [3,3,3]
            InpArgFmt(8)%optional = .FALSE.

            InpArgFmt(9)%IsNumpyArr = .TRUE.
            InpArgFmt(9)%NumpyDType = FYPY_NUMPY_DTYPE_FLOAT32
            InpArgFmt(9)%NumpyArrRank = 4
            InpArgFmt(9)%NumpyArrExtent = [2,2,2]
            InpArgFmt(9)%optional = .FALSE.

            InpArgFmt(10)%IsNumpyArr = .TRUE.
            InpArgFmt(10)%NumpyDType = FYPY_NUMPY_DTYPE_FLOAT64
            InpArgFmt(10)%NumpyArrRank = 2
            InpArgFmt(10)%NumpyArrExtent = [3,5]
            InpArgFmt(10)%optional = .FALSE.

            ! parse inputs
            call fypyobj%ParseTuple(InpArgFmt, intuple, 'TestFyPyParseMethod',&
                                    status, InArgsList)            

            ! print in list
            do listctr = 1, InArgsList%Size()
            block
                character(len=2000) :: astr
                item = InArgsList%Item(listctr)
                
                select type (item)
                type is (integer(C_LONG))
                    write(astr,*) 'long=', item
                type is (integer(C_LONG_LONG))
                    write(astr,*) 'longlong=', item
                type is (real(C_DOUBLE))
                    write(astr,*) 'real=', item
                type is (logical)
                    write(astr,*) 'bool=', item
                type is (complex(kind=C_DOUBLE))
                    write(astr,*) 'complex=', item
                class is (CPtr)
                    inarg = item%ptr
                    if (c_associated(inarg)) then
        
                    if (InpArgFmt(listctr)%IsNumpyArr) then
                        if (InpArgFmt(listctr)%NumpyArrRank == 1 &
                        .AND. InpArgFmt(listctr)%NumpyDType == FYPY_NUMPY_DTYPE_INT8) then
                            call c_f_pointer(inarg, i8arr, InpArgFmt(listctr)%NumpyArrExtent)
                            write(astr,*) 'int8 arr=', i8arr
                        elseif (InpArgFmt(listctr)%NumpyArrRank == 2 &
                            .AND. InpArgFmt(listctr)%NumpyDType == FYPY_NUMPY_DTYPE_INT32) then
                            call c_f_pointer(inarg, i32arr, InpArgFmt(listctr)%NumpyArrExtent)
                            write(astr,*) 'int32 arr=', i32arr
                        elseif (InpArgFmt(listctr)%NumpyArrRank == 3 &
                            .AND. InpArgFmt(listctr)%NumpyDType == FYPY_NUMPY_DTYPE_INT64) then
                            call c_f_pointer(inarg, i64arr, InpArgFmt(listctr)%NumpyArrExtent)
                            write(astr,*) 'int64 arr=', i64arr
                        elseif (InpArgFmt(listctr)%NumpyArrRank == 4 &
                            .AND. InpArgFmt(listctr)%NumpyDType == FYPY_NUMPY_DTYPE_FLOAT32) then
                            call c_f_pointer(inarg, r32arr, InpArgFmt(listctr)%NumpyArrExtent)
                            write(astr,*) 'real32 arr=', r32arr
                        elseif (InpArgFmt(listctr)%NumpyArrRank == 2 &
                            .AND. InpArgFmt(listctr)%NumpyDType == FYPY_NUMPY_DTYPE_FLOAT64) then
                            call c_f_pointer(inarg, r64arr, InpArgFmt(listctr)%NumpyArrExtent)
                            write(astr,*) 'real64 arr=', r64arr
                        end if
                    end if

                    else
                        ! We are in trouble if this is not an optional argument
                        if (.NOT. InpArgFmt(listctr)%optional) then
                            status = FYPY_ERROR_ARGPARSING
                            write(astr,*) 'Parsing: required nump array parsing failed'
                        end if
                    end if
                    
                class default
                    write(astr,*) 'Parse: unknown input type'
                end select
                call FyPyPrint(astr)
            end block
            end do            

            if (status /= FYPY_SUCCESS) then
                ret = C_NULL_PTR
                return
            end if
            

            if (sz == 10) then
            ! extract the tuple objects
            
            ! long integer
            inarg = PyTuple_GetItem(intuple,0_PY_SSIZE_T)
            status = Py2FyType(inarg, l1)
            status = Fy2PyType(l1+1, inarg)
            err = PyTuple_SetItem(outtuple, 0_PY_SSIZE_T, inarg)            
            
            ! long long integer
            inarg = PyTuple_GetItem(intuple,1_PY_SSIZE_T)
            status = Py2FyType(inarg, ll1)
            status = Fy2PyType(ll1+999999999, inarg)
            err = PyTuple_SetItem(outtuple, 1_PY_SSIZE_T, inarg)            
            
            !  real
            inarg = PyTuple_GetItem(intuple,2_PY_SSIZE_T)
            status = Py2FyType(inarg, r1)
            status = Fy2PyType(r1+1.5, inarg)
            err = PyTuple_SetItem(outtuple, 2_PY_SSIZE_T, inarg)            
            
            !  boolean
            inarg = PyTuple_GetItem(intuple,3_PY_SSIZE_T)
            status = Py2FyType(inarg, b1)
            status = Fy2PyType((.NOT. b1), inarg)
            err = PyTuple_SetItem(outtuple, 3_PY_SSIZE_T, inarg)            
            
            ! complex
            inarg = PyTuple_GetItem(intuple,4_PY_SSIZE_T)
            status = Py2FyType(inarg, c1)
            status = Fy2PyType(c1+cmplx(1,1,kind=C_DOUBLE), inarg)
            err = PyTuple_SetItem(outtuple, 4_PY_SSIZE_T, inarg)    
            
            else
               ! error message
               status = Fy2PyType('Need 10 parameters in sequence (long, long long, float, bool, complex', inarg)
               err = PyTuple_SetItem(outtuple, 0_PY_SSIZE_T, inarg)    
            end if

            ret = outtuple
        else
            status = Fy2PyType('Extraction failed', ret)
        end if
        
    end function TestFyPyParseMethod


    function TestFyPyDictMethod(self, args, kwargs) result(ret) bind(C)
        implicit none
        type(C_PTR), value :: self
        type(C_PTR), value    :: args
        type(C_PTR), value    :: kwargs
        type(C_PTR)           :: ret

        integer(C_LONG) :: tmpint, err
        integer(PY_SSIZE_T) :: sz, ctr, kwctr
        
        integer(kind(FYPY_SUCCESS)) :: status
        type(C_PTR) :: intuple, inarg, inkwargs,outtuple, obj
        
        integer(C_LONG) :: l1
        integer(C_LONG_LONG) :: ll1
        real(C_DOUBLE) :: r1
        logical :: b1
        complex(C_DOUBLE) :: c1
        
        ! create out tuple 
        outtuple = PyTuple_New(15_PY_SSIZE_T)
        
        ! extract keyword arguments
        kwctr = 0
        inkwargs = kwargs
        if (c_associated(inkwargs)) then
            if (IsPyDictType(inkwargs,.TRUE.)) then
            ! look for long integer
            obj = PyDict_GetItemString(inkwargs,'l1'//C_NULL_CHAR)
            if (c_associated(obj)) then
                call Py_Incref(obj)
                err = PyTuple_SetItem(outtuple, kwctr, obj)   
                kwctr = kwctr + 1
            end if
            
            ! look for long long integer
            obj = PyDict_GetItemString(inkwargs,'ll1'//C_NULL_CHAR)
            if (c_associated(obj)) then
                call Py_Incref(obj)
                err = PyTuple_SetItem(outtuple, kwctr, obj)   
                kwctr = kwctr + 1
            end if
            
            ! look for real
            obj = PyDict_GetItemString(inkwargs,'r1'//C_NULL_CHAR)
            if (c_associated(obj)) then
                call Py_Incref(obj)                
                err = PyTuple_SetItem(outtuple, kwctr, obj)   
                kwctr = kwctr + 1
            end if
            
            ! look for bool
            obj = PyDict_GetItemString(inkwargs,'b1'//C_NULL_CHAR)
            if (c_associated(obj)) then
                call Py_Incref(obj)                
                err = PyTuple_SetItem(outtuple, kwctr, obj)   
                kwctr = kwctr + 1
            end if
            
            ! look for long integer
            obj = PyDict_GetItemString(inkwargs,'c1'//C_NULL_CHAR)
            if (c_associated(obj)) then
                call Py_Incref(obj)                
                err = PyTuple_SetItem(outtuple, kwctr, obj)   
                kwctr = kwctr + 1
            end if
        end if
        end if
        
        ! check for tuple type
        intuple = args
        sz = pytuple_size(intuple)
        if (ispytupletype(intuple, .true.)) then
        
            ! extract the tuple objects
            do ctr = 1_py_ssize_t, sz
                inarg = pytuple_getitem(intuple, ctr-1) ! borrowed reference
                call Py_Incref(inarg)
                err = pytuple_setitem(outtuple, ctr-1+kwctr, inarg) ! this steals a reference
            end do

            ret = outtuple
        else
            status = Fy2PyType('Extraction failed', ret)
        end if
        

    end function TestFyPyDictMethod





    function TestFyPyCallback(self, args) result(ret) bind(C)
        implicit none
        type(C_PTR), value :: self
        type(C_PTR), value :: args
        type(C_PTR)        :: ret

        integer(C_LONG) :: tmpint, err
        integer(PY_SSIZE_T) :: sz

        integer(kind(FYPY_SUCCESS)) :: status
        type(C_PTR) :: obj, obj1,intuple, inarg, outtuple, cb, cb_res, cb_args, cb_kwargs

        cb_args = PyTuple_New(4_C_LONG_LONG)
        cb_kwargs = PyDict_New()
        
        ! check for tuple type
        intuple = args
        if (IsPyTupleType(intuple, .TRUE.)) then

            ! get callback function object input argument
            cb = PyTuple_GetItem(intuple,0_PY_SSIZE_T)
            call Py_Incref(cb)
            
            ! long integer
            inarg = PyTuple_GetItem(intuple,1_PY_SSIZE_T)
            call Py_Incref(inarg)
            err = PyTuple_SetItem(cb_args, 0_PY_SSIZE_T, inarg)

            ! long long integer
            inarg = PyTuple_GetItem(intuple,2_PY_SSIZE_T)
            call Py_Incref(inarg)
            err = PyTuple_SetItem(cb_args, 1_PY_SSIZE_T, inarg)

            !  real
            inarg = PyTuple_GetItem(intuple,3_PY_SSIZE_T)
            call Py_Incref(inarg)
            err = PyTuple_SetItem(cb_args, 2_PY_SSIZE_T, inarg)

            !  boolean
            inarg = PyTuple_GetItem(intuple,4_PY_SSIZE_T)
            call Py_Incref(inarg)
            err = PyTuple_SetItem(cb_args, 3_PY_SSIZE_T, inarg)

            ! complex
            inarg = PyTuple_GetItem(intuple,5_PY_SSIZE_T)
            call Py_Incref(inarg)
            err = PyDict_SetItemString(cb_kwargs, 'c1'//C_NULL_CHAR, inarg)

        end if

        ! callback
        call FyPyCallPyObject(cb, cb_res, cb_args, cb_kwargs)
        
        ret = cb_res
        
    end function TestFyPyCallback
    



end module TestFyPy
