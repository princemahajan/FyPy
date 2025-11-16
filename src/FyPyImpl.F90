!############################################################################################
!     ______      ____       
!    / ____/_  __/ __ \__  __
!   / /_  / / / / /_/ / / / /
!  / __/ / /_/ / ____/ /_/ / 
! /_/    \__, /_/    \__, /  
!       /____/      /____/   
!
!> \brief       FyPyImpl submodule
!! \details     It provides the implementation of the FyPy module.
!! \author      Bharat Mahajan
!! \date        Created: 08/07/2019    
!
!############################################################################################



submodule (FyPy) FyPyImpl


    contains


    module subroutine FyPyPrintObjects(objs)
        implicit none
        type(C_PTR), dimension(:), intent(in) :: objs
        
        integer(kind(FYPY_SUCCESS)) :: status

        type(C_PTR) :: ret, args, PyPrint
        integer :: err
        integer(PY_SSIZE_T) :: ctr

        status = FYPY_SUCCESS
        
        ! Get the python print function
        PyPrint = PyDict_GetItemString(PyEval_GetBuiltins(),C_CHAR_'print'//C_NULL_CHAR)
        if (.NOT. c_associated(PyPrint)) then
            status = FYPY_ERROR_BUILTIN_NOT_FOUND
        else

            ! Input is an array of PyObjects
            args = PyTuple_New(size(objs,kind=PY_SSIZE_T))
            do ctr = 1, size(objs)
                if (c_associated(objs(ctr))) then
                    call Py_Incref(objs(ctr))
                    err = PyTuple_SetItem(args,ctr-1,objs(ctr))
                end if
            end do

            ! call python print on input tuple
            call FyPyCallPyObject(PyPrint, ret, args)
        
            ! ret is not needed as this is just a print
            call Py_XDecref(ret)
            call Py_Decref(args)
        end if    
    end subroutine FyPyPrintObjects


    
    
    module subroutine FyPyPrintStr(str)
        implicit none
        character(len=*), intent(in) :: str
        
        integer(kind(FYPY_SUCCESS)) :: status
        
        type(C_PTR) :: strobj, ret, args, PyPrint
        integer :: err

        status = FYPY_SUCCESS
        
        ! Get the python print function
        PyPrint = PyDict_GetItemString(PyEval_GetBuiltins(),'print'//C_NULL_CHAR)
        if (.NOT. c_associated(PyPrint)) then
            status = FYPY_ERROR_BUILTIN_NOT_FOUND
        else    
            ! convert string to string object
            status = Fy2PyType(str,strobj)
        
            ! prepare input args tuple, no error checking
            args = PyTuple_New(1_PY_SSIZE_T)
            err = PyTuple_SetItem(args,0_PY_SSIZE_T,strobj)
        
            ! call python print on input tuple
            call FyPyCallPyObject(PyPrint, ret, args)

            ! ret and strobj no longer needed
            call Py_XDecref(ret)
            call Py_Decref(args)
        end if
    end subroutine FyPyPrintStr

    
    

    module subroutine FyPyInit(me, Num_Objects, Num_Methods, Load_NumPy)
        implicit none
        class(FyPyClass), intent(inout) :: me
        integer, intent(in) :: Num_Methods, Num_Objects
        logical :: Load_NumPy

        type(C_PTR) :: obj, obj1, obj2, obj3, numpyobj
        type(PyObject), pointer :: pobj

        integer(C_INT) :: ret

        me%status = FYPY_SUCCESS
        
        ! Get the dictionary of builtins
        me%PyBuiltins = PyEval_GetBuiltins()

        ! Get the python print function
        me%PyPrint = PyDict_GetItemString(me%PyBuiltins, 'print'//C_NULL_CHAR)
        if (.NOT. c_associated(me%PyPrint)) me%status = FYPY_ERROR_BUILTIN_NOT_FOUND

        ! get the string function
        me%str = PyDict_GetItemString(me%PyBuiltins, 'str'//C_NULL_CHAR)
        if (.NOT. c_associated(me%str)) me%status = FYPY_ERROR_BUILTIN_NOT_FOUND

        ! Cache Type objects in global variables for type testing        
        
        ! Get the Py_None object
        Py_None_Object = PyDict_GetItemString(me%PyBuiltins, 'None'//C_NULL_CHAR)
        if (.NOT. c_associated(Py_None_Object)) me%status = FYPY_ERROR_BUILTIN_NOT_FOUND

        ! Intrinsic types

        ! Boolean
        Py_True_Object = PyDict_GetItemString(me%PyBuiltins, 'True'//C_NULL_CHAR)
        if (.NOT. c_associated(Py_True_Object)) me%status = FYPY_ERROR_BUILTIN_NOT_FOUND
        Py_False_Object = PyDict_GetItemString(me%PyBuiltins, 'False'//C_NULL_CHAR)
        if (.NOT. c_associated(Py_False_Object)) me%status = FYPY_ERROR_BUILTIN_NOT_FOUND

        call c_f_pointer(Py_True_Object, pobj)
        Py_Bool_TypeObject = pobj%ob_type
                 
        ! other scalar types
        me%status = Fy2PyType(0_C_Long, obj)
        if (me%status == FYPY_SUCCESS) then
            call c_f_pointer(obj, pobj)
            Py_Long_TypeObject = pobj%ob_type
            call Py_DECREF(obj)
        end if
        me%status = Fy2PyType(0.0_C_DOUBLE, obj)
        if (me%status == FYPY_SUCCESS) then
            call c_f_pointer(obj, pobj)
            Py_Float_TypeObject = pobj%ob_type
            call Py_DECREF(obj)
        end if
        me%status = Fy2PyType(cmplx(1.0,1.0, C_DOUBLE), obj)
        if (me%status == FYPY_SUCCESS) then
            call c_f_pointer(obj, pobj)
            Py_Complex_TypeObject = pobj%ob_type
            call Py_DECREF(obj)
        end if
        me%status = Fy2PyType('FyPy', obj)
        if (me%status == FYPY_SUCCESS) then
            call c_f_pointer(obj, pobj)
            Py_Unicode_TypeObject = pobj%ob_type
            call Py_DECREF(obj)
        end if

        ! Tuple
        obj = PyTuple_New(0_PY_SSIZE_T)
        if (c_associated(obj)) then
            call c_f_pointer(obj, pobj)
            Py_Tuple_TypeObject = pobj%ob_type
            call Py_DECREF(obj)
        end if        

        ! List
        obj = PyList_New(0_PY_SSIZE_T)
        if (c_associated(obj)) then
            call c_f_pointer(obj, pobj)
            Py_List_TypeObject = pobj%ob_type
            call Py_DECREF(obj)
        endif
                
        ! Dictionary
        obj = PyDict_New()
        if (c_associated(obj)) then
            call c_f_pointer(obj, pobj)
            Py_Dict_TypeObject = pobj%ob_type
            call Py_DECREF(obj)
        end if        
        
        ! Exceptions
        PyExc_TypeError = PyDict_GetItemString(me%PyBuiltins, 'TypeError'//C_NULL_CHAR)
        PyExc_ImportError = PyDict_GetItemString(me%PyBuiltins, 'ImportError'//C_NULL_CHAR)

        ! NumPy module Init
        if (Load_NumPy) then
            ! load numpy
            call FyPyImportModule(me%status, me%NumPy%NumPyModule, 'numpy')

            if (me%status == FYPY_SUCCESS) then
                call me%NumPy%Init() ! initialize NumPy
                me%status = me%NumPy%status
            else
                me%status = FYPY_ERROR_PYMODULE_IMPORTFAILED
            end if                
        end if

        ! any error, then do not allocate any storage for methods
        if (me%status == FYPY_SUCCESS) then
            ! allocate storage for module
            allocate(me%pModule)
            ! Allocate Method Table storage
            allocate(me%pMethods(Num_Methods+1))
            ! sentinel
            me%pMethods(Num_Methods+1) = PyMethodDef(ml_name = C_NULL_PTR, &
                                                 ml_meth = C_NULL_FUNPTR, &
                                                 ml_flags = 0, &
                                                 ml_doc = C_NULL_PTR)            
        end if
    end subroutine FyPyInit


    !> NumPy Initialization
    module subroutine NumPyInit(me)
        implicit none
        class(NumPyClass), intent(inout) :: me

        type(C_PTR) :: obj, obj1, obj2, obj3, numpyobj
        type(PyObject), pointer :: pobj
        integer(C_INT) :: error

        me%status = FYPY_SUCCESS
        
        ! save numpy array type
        numpyobj = PyObject_GetAttrString(me%NumPyModule, 'identity'//C_NULL_CHAR)
        obj = PyTuple_New(1_PY_SSIZE_T)
        me%status = Fy2PyType(3_C_LONG,obj1)
        error = PyTuple_SetItem(obj, 0_PY_SSIZE_T, obj1)

        call FyPyCallPyObject(numpyobj, obj2, obj)
        
        if (c_associated(obj2)) then
            ! get the ndarry type object to be used later for type checking
            call c_f_pointer(obj2, pobj)
            Py_Array_TypeObject = pobj%ob_type
        else
            me%status = FYPY_ERROR_PYCALL
        end if
            
        call Py_XDecref(numpyobj)
        call Py_XDecref(obj)
        call Py_XDecref(obj2)
        
        ! save the numpy array data types
        me%status = Fy2PyType('int8',me%NUMPY_DTYPE_INT8)
        me%status = Fy2PyType('int32',me%NUMPY_DTYPE_INT32)
        me%status = Fy2PyType('int64',me%NUMPY_DTYPE_INT64)
        me%status = Fy2PyType('float32',me%NUMPY_DTYPE_FLOAT32)
        me%status = Fy2PyType('float64',me%NUMPY_DTYPE_FLOAT64)
        me%status = Fy2PyType('complex128',me%NUMPY_DTYPE_COMPLEX128)

        ! save the ndarray method for future calls
        me%ndarray = PyObject_GetAttrString(me%NumPyModule, 'ndarray'//C_NULL_CHAR)
            
        if (.NOT. c_associated(me%ndarray)) then
            me%status = FYPY_ERROR_NUMPY_BUILTIN_ERROR
        end if

    end subroutine NumPyInit





    module subroutine FyPyAddMethod(me, name, docstr, &
                NoArgsMethod, ArgsMethod, ArgsKwMethod, FastMethod, FastKwMethod)
        implicit none
        class(FyPyClass), intent(inout) :: me
        character(kind=C_CHAR,len=*), intent(in) :: name
        character(kind=C_CHAR,len=*), intent(in) :: docstr

        procedure(), optional           :: NoArgsMethod
        procedure(), optional           :: ArgsMethod
        procedure(), optional           :: ArgsKwMethod
        procedure(), optional           :: FastMethod
        procedure(), optional           :: FastKwMethod

        integer(kind(FYPY_METHODCALL_NOARGS)) :: calltype 
        type(C_FUNPTR) :: cmethodptr

        character(kind=C_CHAR, len=:), pointer :: cname => null()
        character(kind=C_CHAR, len=:), pointer :: cdocstr => null()
        integer(C_INT) :: flags

        integer :: ctr
        
        me%status = FYPY_SUCCESS

        ! check for function arguments
        ctr = 0
        if (present(NoArgsMethod)) then
            calltype = FYPY_METHODCALL_NOARGS
            cmethodptr = c_funloc(NoArgsMethod)
            ctr = ctr + 1
        end if
        
        if (present(ArgsMethod)) then
            calltype = FYPY_METHODCALL_VARARGS
            cmethodptr = c_funloc(ArgsMethod)
            ctr = ctr + 1
        end if
        
        if (present(ArgsKwMethod)) then
            calltype = FYPY_METHODCALL_VARGS_KEYWORDS
            cmethodptr = c_funloc(ArgsKwMethod)
            ctr = ctr + 1
        end if
        
        if (present(FastMethod)) then
            calltype = FYPY_METHODCALL_FAST
            cmethodptr = c_funloc(FastMethod)
            ctr = ctr + 1
        end if
        
        if (present(FastKwMethod)) then
            calltype = FYPY_METHODCALL_FAST_KEYWORDS
            cmethodptr = c_funloc(FastKwMethod)
            ctr = ctr + 1
        end if
        
        if (ctr /= 1) then
            ! Either none or more than 1 method argument is provided
            me%status = FYPY_ERROR_PARAMS
            return
        end if
        
        ! increment method table counter
        me%ExportedMethods = me%ExportedMethods + 1

        ! allocate storage name and docstr strings
        allocate(character(kind=C_CHAR, len=len(name)+1) :: cname)
        allocate(character(kind=C_CHAR, len=len(docstr)+1) :: cdocstr)

        ! copy c strings
        cname = name//C_NULL_CHAR
        cdocstr = docstr//C_NULL_CHAR

        ! flags for this method call type
        select case (calltype)

        case (FYPY_METHODCALL_NOARGS)
            flags = METH_NOARGS
        case (FYPY_METHODCALL_VARARGS)
            flags = METH_VARARGS
        case (FYPY_METHODCALL_VARGS_KEYWORDS)
            flags = METH_VARARGS + METH_KEYWORDS
#ifdef Py_LIMITED_API            
        case (FYPY_METHODCALL_FAST)
            flags =METH_FASTCALL
        case (FYPY_METHODCALL_FAST_KEYWORDS)
            flags = METH_FASTCALL + METH_KEYWORDS
#endif            
        case (FYPY_METHODCALL_O)
            flags = METH_O
        case default
            flags = 0

        end select

        if (flags == 0) then
            me%status = FYPY_ERROR_PARAMS
        end if

        ! create entry in method table
        if (me%status == FYPY_SUCCESS)  then
            me%pMethods(me%ExportedMethods) = PyMethodDef(ml_name   = c_loc(cname), &
                                                      ml_meth   = cmethodptr, &
                                                      ml_flags  = flags, &
                                                      ml_doc    = c_loc(cdocstr))
        end if            

    end subroutine FyPyAddMethod



    module subroutine FyPyAddObject(me, ModuleObj, name, obj)
        implicit none
        class(FyPyClass), intent(inout) :: me
        type(C_PTR), value :: ModuleObj
        character(kind=C_CHAR, len=*), intent(in) :: name
        type(C_PTR), value :: obj

        integer(C_INT) :: error
        character(kind=C_CHAR, len=:), pointer :: cname => null()

        ! increment method table counter
        me%ExportedObjects = me%ExportedObjects + 1

        ! increment refcount first as Python_AddObject steals a reference
        call Py_Incref(obj)
        if (c_associated(obj)) then
            error = PyModule_AddObject(ModuleObj, name//C_NULL_CHAR, obj)
        end if

    end subroutine FyPyAddObject



    module subroutine FyPyCreateTypeObject(me, TypeObj, tp_name, tp_basicsize, VarSizeObj,&
                                tp_flags, docstr, tp_new_handler)
        implicit none
        class(FyPyClass), intent(inout) :: me
        type(C_PTR), value :: TypeObj
        
        character(len=*), intent(in), target, optional :: tp_name
        integer(PY_SSIZE_T), intent(in), optional :: tp_basicsize
        logical, intent(in), optional :: VarSizeObj
        integer(C_LONG), intent(in), optional :: tp_flags
        character(len=*), intent(in), target, optional :: docstr
        type(C_FUNPTR), optional :: tp_new_handler

        type(PyTypeObject), pointer :: pTypeObj

        ! get the fortran pointers
        call c_f_pointer(TypeObj, pTypeObj)

        ! set the string for textual representation
        pTypeObj%tp_name = c_loc(tp_name)

        ! set the basic size to tell CPython how much memory to allocate
        pTypeObj%tp_basicsize = tp_basicsize

        ! size=0 if object is not variable sized
        if (present(VarSizeObj)) then
            if (.NOT. VarSizeObj) pTypeObj%tp_itemsize = 0
        else
            pTypeObj%tp_itemsize = 0 ! default not variable size object
        end if
        
        ! set TP flags
        if (present(tp_flags)) then
            pTypeObj%tp_flags = tp_flags
        else
            pTypeObj%tp_flags = Py_TPFLAGS_DEFAULT
        end if

        ! set doc string
        if (present(docstr)) pTypeObj%tp_doc = c_loc(docstr)

        ! object creation handler
        if (present(tp_new_handler)) then
            pTypeObj%tp_new = tp_new_handler
        else
            pTypeObj%tp_new = c_funloc(PyType_GenericNew)
        end if

    end subroutine FyPyCreateTypeObject



    module subroutine FyPyImportModule(status, modobj, modname, modpath, fromlist, globaldict, localdict, Reload)
        implicit none
        integer(kind(FYPY_SUCCESS)), intent(out) :: status !< Python callback function object
        type(C_PTR), intent(out) :: modobj
        character(kind=C_CHAR,len=*), intent(in) :: modname
        character(len=*), intent(in), optional :: modpath
        character(len=100), dimension(:), intent(in), optional :: fromlist
        type(C_PTR), value, optional :: globaldict
        type(C_PTR), value, optional :: localdict
        logical, intent(in), optional :: Reload

        logical :: ModPathExist, OnlyReload
        
        type(C_PTR) :: nameobj, pathlist, pathobj, fromlistobj, flobj, globalobj, localobj, attr, args,ret

        integer(C_INT) :: error
        integer(PY_SSIZE_T) :: ctr

        ! parameter checking

        status = FYPY_SUCCESS
        
        ModPathExist = .FALSE.
        if (present(modpath)) then
            ModPathExist = .TRUE.
        end if

        if (present(fromlist)) then
            ! create fromlist object
            fromlistobj = PyTuple_New(size(fromlist,kind=PY_SSIZE_T))
            ! add fromlist items one by one
            do ctr = 1_PY_SSIZE_T,size(fromlist)
                status = Fy2PyType(fromlist(ctr)(1:len_trim(fromlist(ctr))), flobj)
                if (status == FYPY_SUCCESS) then
                    ! No check for error as we add enough fromlist items as we can
                    error = PyTuple_SetItem(fromlistobj,ctr-1,flobj) 
                end if
            end do
        else
            ! create empty fromlist object
            fromlistobj = PyTuple_New(0_PY_SSIZE_T)
        end if

       if (present(globaldict)) then
            globalobj = globaldict
       else
            call Py_Incref(Py_None_Object)
            globalobj = Py_None_Object

        end if
       
        if (present(localdict)) then
            localobj = localdict
        else
            call Py_Incref(Py_None_Object)
            localobj = Py_None_Object
        end if
       
        OnlyReload = .FALSE.
        if (present(Reload)) then
            OnlyReload = Reload
        end if
       
        ! get the string object for module name
        status = Fy2PyType(modname, nameobj)

        ! handle module reload first
        if (OnlyReload .AND. (status == FYPY_SUCCESS)) then
            ! get the already loaded module object, new reference
            modobj = PyImport_GetModule(nameobj)
       
            if (.NOT. c_associated(modobj)) then
                status = FYPY_ERROR_PYMODULE_NOTFOUND
            end if
       
            ! reload the module
            if (status == FYPY_SUCCESS) then
                ! reload and this increments reference count again
                modobj = PyImport_ReloadModule(modobj)
                if (.NOT. c_associated(modobj)) then
                    status = FYPY_ERROR_PYMODULE_IMPORTFAILED
                else
                    ! decrease reference count by 1
                    call Py_Decref(modobj)
                end if    
            end if
       
            ! only reload needed, so return
            return
        end if
       

        ! set module path in sys.path if given 
        if (ModPathExist .AND. (status == FYPY_SUCCESS)) then
            ! get the path list
            pathlist = PySys_GetObject('path'//C_NULL_CHAR)
            
            ! append the provided path name to the list
            status = Fy2PyType(modpath, pathobj)
            if (status == FYPY_SUCCESS) error = PyList_Append(pathlist, pathobj)
            if (error /= 0) status = FYPY_ERROR_PYTHONEXCEPTION
        end if
       
        if (status == FYPY_SUCCESS) then
            ! make the call to load the module now
            modobj = PyImport_ImportModuleLevelObject(nameobj,globalobj,localobj,fromlistobj,0)
        else
            modobj = C_NULL_PTR
        end if
        
        if (.NOT. c_associated(modobj)) status = FYPY_ERROR_PYMODULE_IMPORTFAILED
        
        ! reference cleanup
        call Py_XDecref(fromlistobj)
        call Py_Decref(Py_None_Object)
        call Py_Decref(Py_None_Object)

    end subroutine FyPyImportModule






    module subroutine FyPyCallPyObject(PyFunObj, ret, args, kwargs)
        implicit none
        type(C_PTR), value :: PyFunObj !< Python callback function object
        type(C_PTR), intent(out) :: ret
        type(C_PTR), value, optional :: args
        type(C_PTR), value, optional :: kwargs

        logical :: Args_Needed, KwArgs_Needed
        integer :: error
        type(C_PTR) :: argstuple

        Args_Needed = .FALSE.
        if (present(args)) then
            if (c_associated(args)) Args_Needed = .TRUE.
        end if

        KwArgs_Needed = .FALSE.
        if (present(kwargs)) then
            if (c_associated(kwargs)) KwArgs_Needed = .TRUE.
        end if

        !argstuple = args
        if (.NOT. Args_Needed) then
            argstuple = PyTuple_New(0_PY_SSIZE_T)
        else
            argstuple = args  
        end if
            
        ! Check if the Function object is callable
        if (PyCallable_Check(PyFunObj) /= 1) then
            ! raise exception
            call PyErr_SetString(PyExc_TypeError, 'Python callback object not callable')
            ret = C_NULL_PTR
        else
            ! Make the python call
            if (Args_Needed .AND. KwArgs_Needed) then
                ret = PyObject_Call(PyFunObj, argstuple, kwargs)
            else if (Args_Needed .AND. (.NOT. KwArgs_Needed)) then
                ret = PyObject_CallObject(PyFunObj, argstuple)
            else if ((.NOT. Args_Needed) .AND. KwArgs_Needed) then
                ret = PyObject_Call(PyFunObj, argstuple, kwargs)
            else
                ret = PyObject_CallObject(PyFunObj, C_NULL_PTR)
            end if
        end if
    end subroutine FyPyCallPyObject



    type(C_PTR) module function FyPyCreatePyModule(me, name, docstr)
        implicit none
        class(FyPyClass), intent(inout) :: me
        character(kind=C_CHAR,len=*), intent(in) :: name
        character(kind=C_CHAR,len=*), intent(in) :: docstr

        character(kind=C_CHAR, len=:), pointer :: cname !=> null()
        character(kind=C_CHAR, len=:), pointer :: cdocstr !=> null()

        ! allocate storage name and docstr strings
        allocate(character(kind=C_CHAR, len=len(name)+1) :: cname)
        allocate(character(kind=C_CHAR, len=len(docstr)+1) :: cdocstr)
        cname = name//C_NULL_CHAR
        cdocstr = docstr//C_NULL_CHAR

        ! create module struct
        me%pModule = PyModuleDef(m_base     = PyModuleDef_Base(&
                                                    ob_base = PyObject(&
                                                                    ob_refcnt  = 1_PY_SSIZE_T, &
                                                                    ob_type    = C_NULL_PTR), &
                                                    m_init     = C_NULL_FUNPTR, &
                                                    m_index    = 0, &
                                                    m_copy     = C_NULL_PTR),&
                                m_name      = c_loc(cname), &
                                m_doc       = c_loc(cdocstr), &
                                m_size      = -1_PY_SSIZE_T, &
                                m_methods   = c_loc(me%pMethods), &
                                m_slots     = C_NULL_PTR, &
                                m_traverse  = C_NULL_FUNPTR, &
                                m_clear     = C_NULL_FUNPTR, &
                                m_free      = C_NULL_FUNPTR)

        ! create python module
        FyPyCreatePyModule = PyModule_Create2(c_loc(me%pModule),PYTHON_API_VERSION)

        ! store the fortran pointer in the object (dont know for what!)
        call c_f_pointer(FyPyCreatePyModule, me%pModuleObj)

    end function FyPyCreatePyModule



    !> Parse a tuple and returns arguments converted to Fortran types in a list
    !! TBD: Modify Py2FyArray function to return directly the fortran array
    !! instead of its c pointer and return the fortran type in the list
    module subroutine FyPyParseTuple(me, fmt, PyTuple, ErrStr, status, ArgsList)
        implicit none
        class(FyPyClass), intent(inout) :: me
        
        !> Expected Format specifier for each of the element in thd tuple
        type(FyPyMethodArgsFormat), dimension(:), intent(inout) :: fmt
        !> Python tuple to parse
        type(C_PTR), value :: PyTuple
        !> In case of parsing error, this string is prepended to the error message
        character(len=*), intent(in) :: ErrStr
        !> FYPY_SUCCESS on successful parsing
        integer(kind(FYPY_SUCCESS)), intent(out) :: status
        !> All the converted arguments in this list
        type(List), intent(out)     :: ArgsList

        integer(PY_SSIZE_T) :: ctr
        integer :: itr, index
        type(C_PTR) :: inarg, outarg

        logical                         :: BoolArg
        integer(C_LONG)                 :: LongArg
        integer(C_LONG_LONG)            :: LongLongArg
        real(C_DOUBLE)                  :: DoubleArg
        complex(C_DOUBLE)               :: ComplexDoubleArg
        character(len=:), allocatable   :: StrArg
        
        integer(kind(FYPY_NUMPY_DTYPE_FLOAT64)) :: NumpyArrType
        integer, dimension(:), allocatable :: NumpyArrShape

        type(CPtr) :: CPtrArg

        logical :: IsArgValid

        character(kind=C_CHAR, len=:), allocatable :: ErrMsg
        integer :: ErrMsgLen

        character(kind=C_CHAR, len=*), parameter :: ErrMsgSubstr = &
                            ' missing required positional argument: '

        ! parse the tuple one by one
        do ctr = 1, size(fmt)
            
            ! extract the arg passed
            inarg = PyTuple_GetItem(PyTuple, ctr-1) ! borrowed reference

            ! convert the tuple object to the corresponding Fortran type
            status = FYPY_ERROR_PARAMS
            IsArgValid = .FALSE.
            
            if (.NOT. fmt(ctr)%IsNumpyArr) then
            
               ! check for recognized type and try to convert it
               ! if successful, save it in the list
            
               select case (fmt(ctr)%PyObjType)
               case (FYPY_BOOL)
                   if (IsPyBoolType(inarg)) then
                       status = Py2FyType(inarg, BoolArg)
                       if (status == FYPY_SUCCESS) then
                           IsArgValid = .TRUE.
                           index = ArgsList%PushBack(BoolArg)
                       end if
                   end if
               case (FYPY_LONG)
                   if (IsPyLongType(inarg,.True.)) then
                       status = Py2FyType(inarg, LongArg)
                       if (status == FYPY_SUCCESS) then
                           IsArgValid = .TRUE.
                           index = ArgsList%PushBack(LongArg)
                       end if
                   end if
               case (FYPY_LONG_LONG_INT)
                   if (IsPyLongType(inarg,.True.)) then
                       status = Py2FyType(inarg, LongLongArg)
                       if (status == FYPY_SUCCESS) then
                           IsArgValid = .TRUE.
                           index = ArgsList%PushBack(LongLongArg)
                       end if
                   end if
               case (FYPY_DOUBLE)
                   if (IsPyFloatType(inarg,.True.)) then
                       status = Py2FyType(inarg, DoubleArg)
                       if (status == FYPY_SUCCESS) then
                           IsArgValid = .TRUE.
                           index = ArgsList%PushBack(DoubleArg)
                       end if
                   end if
               case (FYPY_COMPLEX_DOUBLE)
                   if (IsPyComplexType(inarg,.True.)) then
                       status = Py2FyType(inarg, ComplexDoubleArg)
                       if (status == FYPY_SUCCESS) then
                           IsArgValid = .TRUE.
                           index = ArgsList%PushBack(ComplexDoubleArg)
                       end if
                   end if
               case (FYPY_UNICODE_STRING)
                   if (IsPyUnicodeType(inarg,.True.)) then
                       status = Py2FyType(inarg, StrArg)
                       if (status == FYPY_SUCCESS) then
                           IsArgValid = .TRUE.
                           index = ArgsList%PushBack(StrArg)
                       end if
                   end if
               case default
                   status = FYPY_ERROR_PARAMS
               end select
            
               ! If arg conversion failed, stop parsing
               if ((.NOT. IsArgValid) .AND. fmt(ctr)%optional) then
                   ! If this is an optional argument, return null pointer
                   ! without raising an exception
                   IsArgValid = .TRUE.
                   outarg = C_NULL_PTR
                   index = ArgsList%PushBack(outarg)
               end if
            
            else
            
               ! check for recognized numpy array and try to convert it
               ! If successful, save it in the list
               
               if (IsPyArrayType(inarg, .True.)) then
                   
                   status = me%Py2FyArray(inarg, NumpyArrType, &
                                               NumpyArrShape, outarg)                    

                    ! check for valid rank and shape of the Numpy array
                   if (status == FYPY_SUCCESS &
                       .AND. NumpyArrType == fmt(ctr)%NumpyDType &
                       .AND. size(NumpyArrShape) == fmt(ctr)%NumpyArrRank) then

                       ! The received Numpy array is of valid rank
                       ! Now check for its extent along requested dimensions
                       IsArgValid = .TRUE.
                       if (allocated(fmt(ctr)%NumpyArrExtent)) then
                           do itr = 1, min(size(fmt(ctr)%NumpyArrExtent),size(NumpyArrShape))
                               if (fmt(ctr)%NumpyArrExtent(itr) /= NumpyArrShape(itr)) then
                                   IsArgValid = .FALSE.
                               end if
                           end do
                       end if
                   end if
               end if
               
               if (IsArgValid) then
                    ! Update the array shape and add its C pointer to the list
                    CPtrArg%ptr = outarg
                    fmt(ctr)%NumpyArrExtent = NumpyArrShape
                    index = ArgsList%PushBack(CPtrArg)
               elseif ((.NOT. IsArgValid) .AND. fmt(ctr)%optional) then
                    ! If this is an optional argument, return null pointer
                    ! without raising an exception
                    IsArgValid = .TRUE.
                    outarg = C_NULL_PTR
                    CPtrArg%ptr = outarg
                    index = ArgsList%PushBack(CPtrArg)
               end if
            
            end if
            
            ! exit parsing if a mandatory argument parsing failed
            if (.NOT. IsArgValid) exit

        end do

        if (.NOT. IsArgValid) then
            ! build error message
            ErrMsgLen = len(ErrStr) + len(ErrMsgSubstr) + 2
            allocate(character(kind=C_CHAR,len=ErrMsgLen) :: ErrMsg)
            write(ErrMsg, "(2A,I2)") ErrStr, ErrMsgSubstr, ctr
            ! raise a type error exception now
            ! TBD: This is not working!
            call PyErr_SetString(PyExc_TypeError, ErrMsg//C_NULL_CHAR)
            call FyPyPrint(ErrMsg//C_NULL_CHAR)
        end if

    end subroutine FyPyParseTuple




    !> NumPy class destructor
    module subroutine NumPyDestroy(me)
        implicit none
        type(NumPyClass), intent(inout) :: me
        
        ! free memory
        call Py_XDecref(me%ndarray)
        call Py_XDecref(me%NumPyModule)
    end subroutine NumPyDestroy


    !> FyPy class destructor
    module subroutine FyPyDestroy(me)
        implicit none
        type(FyPyClass), intent(inout) :: me
        
        ! free memory

        if (associated(me%pModule)) deallocate(me%pModule)
        me%pModule => null()

        if (associated(me%pMethods)) deallocate(me%pMethods)
        me%pMethods => null()

        call Py_XDecref(c_loc(me%pModuleObj))
        me%pModuleObj => null()

    end subroutine FyPyDestroy
    









end submodule FyPyImpl




