!############################################################################################
!
!  FyPy
!
!> \brief       ModuleSupport module
!! \details     It provides Fortran equivalent of Python/C APIs useful for implementing Python
!!              extension modules in additiont to embedding Python.
!! \author      Bharat Mahajan
!! \date        08/07/2019    
!
!############################################################################################


module ModuleSupport

    use, intrinsic :: iso_c_binding, only: C_PTR, C_FUNPTR, C_INT, C_CHAR, C_LONG, &
                            C_NULL_PTR, C_NULL_FUNPTR
    use Object, only: PyObject, PY_SSIZE_T
 
    implicit none

    public


    !> Flags passed to newmethodobject, see CPython's methodobject.h
    integer, parameter :: METH_VARARGS = int(z'0001') !< Calling convention for the type PyCFunction
    integer, parameter :: METH_KEYWORDS = int(z'0002') !< Calling convention for the type PyCFunctionWithKeywords

    !> METH_NOARGS and METH_O must not be combined with the flags above
    integer, parameter :: METH_NOARGS = int(z'0004') !< For methods without parameters 
    integer, parameter :: METH_O = int(z'0008') !< For methods with a single object argument
    
    !> METH_CLASS and METH_STATIC are a little different; these control
    !!   the construction of methods for a class.  These cannot be used for
    !!   functions in modules.
    !! Method will be passed the type object as the first parameter rather than an instance of the type.
    !! This is used to create class methods.
    integer, parameter :: METH_CLASS = int(z'0010')
    !> The method will be passed NULL as the first parameter rather than an instance of the type. 
    !! This is used to create static methods.
    integer, parameter :: METH_STATIC = int(z'0020')

    !> METH_COEXIST allows a method to be entered even though a slot has
    !!   already filled the entry.  When defined, the flag allows a separate
    !!   method, "__contains__" for example, to coexist with a defined
    !!   slot like sq_contains.
    integer, parameter :: METH_COEXIST = int(z'0040')
    
#ifdef Py_LIMITED_API
    integer, parameter :: METH_FASTCALL = int(z'0080') !< Fast calling convention supporting only positional arguments
#endif
    
    ! This bit is preserved for Stackless Python
#ifdef STACKLESS
    integer, parameter :: METH_STACKLESS = int(z'0100')    
#else
    integer, parameter :: METH_STACKLESS = int(z'0000')    
#endif


    !> Module Definition structures: see CPython's moduleobject.h
    type, bind(C) :: PyModuleDef_Base
        type(PyObject) :: ob_base
        type(C_FUNPTR) :: m_init            = C_NULL_FUNPTR
        integer(kind=PY_SSIZE_T) :: m_index = 0
        type(C_PTR) :: m_copy               = C_NULL_PTR
    end type PyModuleDef_Base

    !> Module Definition structures: see CPython's moduleobject.h 
    type, bind(C) :: PyModuleDef
        type(PyModuleDef_Base) :: m_base
        type(C_PTR) :: m_name               = C_NULL_PTR
        type(C_PTR) :: m_doc                = C_NULL_PTR
        integer(kind=PY_SSIZE_T) :: m_size  = 0
        type(C_PTR) :: m_methods            = C_NULL_PTR
        type(C_PTR) :: m_slots              = C_NULL_PTR
        type(C_FUNPTR) :: m_traverse        = C_NULL_FUNPTR
        type(C_FUNPTR) :: m_clear           = C_NULL_FUNPTR
        type(C_FUNPTR) :: m_free            = C_NULL_FUNPTR
    end type PyModuleDef
  

    interface

    !> Creates a new module object and return it directly. This is for 
    !! single-phase initialization of modules only.
    type(C_PTR) function PyModule_Create(moddef) bind(C, name='PyModule_Create')
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: moddef
    end function PyModule_Create

    !> This sets the Python API Version in addition to doing what PyModule_Create does
    type(C_PTR) function PyModule_Create2(moddef, Mod_API_Version) bind(C, name="PyModule_Create2")
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: moddef
        integer(kind=C_INT), value :: Mod_API_Version
    end function PyModule_Create2

    !> Add one object with name and value (object) to the module object.
    !! Return -1 on error, 0 on success.
    integer(C_INT) function PyModule_AddObject(mod, name, value) bind(c, name='PyModule_AddObject')
        import :: C_PTR, C_CHAR, C_INT
        type(C_PTR), value :: mod
        character(kind=C_CHAR,len=1), dimension(*), intent(in) :: name
        type(c_ptr), value :: value
    end function PyModule_AddObject
      
    !> Add one object with name and value (object) to the module object.
    !! Return -1 on error, 0 on success.
    integer(C_INT) function PyModule_AddIntConstant(mod, name, value) bind(c, name='PyModule_AddIntConstant')
        import :: C_PTR, C_LONG, C_INT, C_CHAR
        type(C_PTR), value :: mod
        character(kind=C_CHAR), dimension(*),intent(in) :: name
        integer(C_LONG), value :: value
    end function PyModule_AddIntConstant

    !##################
    ! Importing Modules
    !##################

    !> Imports a module with everything. New reference
    type(C_PTR) function PyImport_ImportModule(name) bind(C, name='PyImport_ImportModule')
       import :: C_PTR, C_CHAR
       character(kind=C_CHAR), dimension(*), intent(in) :: name
    end function PyImport_ImportModule

    !> Imports a module with everything. New reference
    type(C_PTR) function PyImport_Import(nameobj) bind(C, name='PyImport_Import')
       import :: C_PTR
       type(C_PTR), value :: nameobj
    end function PyImport_Import
    
    !> Imports a module, similar to builtin __import__(). New reference
    type(C_PTR) function PyImport_ImportModuleEx(name,globals,locals,fromlist) &
                                bind(C, name='PyImport_ImportModuleEx')
       import :: C_PTR, C_CHAR
       character(kind=C_CHAR), dimension(*), intent(in) :: name
       type(C_PTR), value :: globals !< Python Dictionary of globals
       type(C_PTR), value :: locals  !< Python Dictionary of locals
       type(C_PTR), value :: fromlist !< Python List of names of attributes to import 
    end function PyImport_ImportModuleEx

    
    !> Imports a module, similar to builtin __import__(). New reference
    type(C_PTR) function PyImport_ImportModuleLevel(name,globals,locals,fromlist,level) &
                                bind(C, name='PyImport_ImportModuleLevel')
       import :: C_PTR, C_CHAR
       character(kind=C_CHAR), dimension(*), intent(in) :: name
       type(C_PTR), value :: globals !< Python Dictionary of globals
       type(C_PTR), value :: locals  !< Python Dictionary of locals
       type(C_PTR), value :: fromlist !< Python List of names of attributes to import 
       integer :: level
    end function PyImport_ImportModuleLevel    
    
    
    !> Imports a module, Standard __import__() calls this function internally. New reference
    type(C_PTR) function PyImport_ImportModuleLevelObject(name,globals,locals,fromlist, level) &
                        bind(C, name='PyImport_ImportModuleLevelObject')
       import :: C_PTR, C_INT, C_CHAR
       type(C_PTR), value :: name
       type(C_PTR), value :: globals !< Python Dictionary of globals
       type(C_PTR), value :: locals  !< Python Dictionary of locals
       type(C_PTR), value :: fromlist !< Python List of names of attributes to import 
       integer(C_INT), value :: level   !< level of parent directories to search for
    end function PyImport_ImportModuleLevelObject

    !> Reloads a module. New reference. Returns NULL on failure
    type(C_PTR) function PyImport_ReloadModule(modobj) bind(C, name='PyImport_ReloadModule')
       import :: C_PTR
       type(C_PTR), value :: modobj
    end function PyImport_ReloadModule

    !> Returns a module. New reference. Returns NULL and set exception 
    !! if lookup failed. If module has not been imported then just return NULL
    type(C_PTR) function PyImport_GetModule(nameobj) bind(C, name='PyImport_GetModule')
       import :: C_PTR
       type(C_PTR), value :: nameobj
    end function PyImport_GetModule

    !> Returns the object name from sys module or NULL if doesnt exist. Borrowed reference
    type(C_PTR) function PySys_GetObject(name) bind(C, name='PySys_GetObject')
       import :: C_PTR, C_CHAR
       character(kind=C_CHAR), dimension(*), intent(in) :: name
    end function PySys_GetObject

    !> Set name in sys module to v unless v is NULL, in which case v is deleted from sys.
    !! Returns 0 on success and -1 on error.
    type(C_PTR) function PySys_SetObject(name, v) bind(C, name='PySys_SetObject')
       import :: C_PTR, C_CHAR
       character(kind=C_CHAR), dimension(*), intent(in) :: name
       type(C_PTR), value :: v
    end function PySys_SetObject

    !> Returns a dictionary of the builtins. Borrowed reference
    type(C_PTR) function PyEval_GetBuiltins() bind(C, name='PyEval_GetBuiltins')
       import :: C_PTR
    end function PyEval_GetBuiltins


    !################
    ! Process Control
    !################
    
    !> Prints a fatal error message and kills the process. no cleanup is performed.
    subroutine Py_FatalError(message) bind(C, name='Py_FatalError')
        import :: C_CHAR
        implicit none
        character(kind=C_CHAR), dimension(*), intent(in) :: message
    end subroutine Py_FatalError
    
    !> Exits the current process.
    subroutine Py_Exit(status) bind(C, name='Py_Exit')
        import :: C_INT
        implicit none
        integer(C_INT), value :: status
    end subroutine Py_Exit

    !> Runs command, returns 0 on success or -1 on failure.
    integer(C_INT) function PyRun_SimpleString(command) bind(C, name='PyRun_SimpleString')
        import :: C_INT, C_CHAR
        character(kind=C_CHAR), dimension(*), intent(in) :: command
    end function PyRun_SimpleString
    
    
    subroutine Py_Initialize() bind(C, name='Py_Initialize')
    end subroutine Py_Initialize


    end interface


    abstract interface
    
    ! Callable method interfaces, see CPython's methodobject.h
    
    !> Simplest Python callable method signature with no arguments
    function PyNoArgsFunction(self) result(ret) bind(C)
        import :: C_PTR
        implicit none
        type(C_PTR), value :: self   !< Module object for module-level functions or the object
                                                !! instance in case of a method
        type(C_PTR)        :: ret    !< Return object. Return NULL in case of an exception
    end function PyNoArgsFunction
    
    !> Typical Python callable method signature. Must return a new reference.
    function PyCFunction(self, args) result(ret) bind(C)
        import :: C_PTR
        implicit none
        type(C_PTR), value :: self   !< Module object for module-level functions or the object
                                                !! instance in case of a method
        type(C_PTR), value :: args   !< Python tuple object containing the arguments
        type(C_PTR)        :: ret    !< Return object. Return NULL in case of an exception
    end function PyCFunction
    
    !> Python callable method signature corresponding to flags: METH_VARARGS | METH_KEYWORDS
    function PyCFunctionWithKeywords(self, args, kwargs) result(ret) bind(C)
        import :: C_PTR
        implicit none
        type(C_PTR), value :: self   !< Module object or an object instance to whom the function/method belongs
        type(C_PTR), value :: args   !< Tuple object for all arguments
        type(C_PTR), value :: kwargs !< Dictionary of all the keyword arguments or NULL
        type(C_PTR)        :: ret    !< Return object. Return NULL in case of an exception
    end function PyCFunctionWithKeywords
    
    !> Python callables in C with signature defined by flag METH_FASTCALL
    function PyCFunctionFast(self, argarray, nargs) result(ret) bind(C)
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value              :: self     !< Module object or an object instance to whom the function/method belongs
        type(C_PTR), value              :: argarray !< C array of PyObject* values indicating the arguments
        integer(kind=PY_SSIZE_T), value :: nargs    !< Number of arguments
        type(C_PTR)                     :: ret      !< Return object. Return NULL in case of an exception        
    end function PyCFunctionFast
    
    !> Python callables in C with signature defined by flag METH_FASTCALL | METH_KEYWORDS
    function PyCFunctionFastWithKeywords(self, argarray, nargs, kwargs) result(ret) bind(C)
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value              :: self     !< Module object or an object instance to whom the function/method belongs
        type(C_PTR), value              :: argarray !< C array of PyObject* values indicating the arguments
        integer(kind=PY_SSIZE_T), value :: nargs    !< Number of arguments
        type(C_PTR), value              :: kwargs   !< Dictionary of all the keyword arguments or NULL
        type(C_PTR)                     :: ret      !< Return object. Return NULL in case of an exception        
    end function PyCFunctionFastWithKeywords
    
    end interface


    !> Built-in Module Method objects, see CPythons's methodobject.h
    !! Python extensions use this to describe a callable method
    type, bind(C) :: PyMethodDef
        !> The name of the built-in function/method
        type(C_PTR) :: ml_name          = C_NULL_PTR   
        !> The C function that implements it      
        type(C_FUNPTR) :: ml_meth       = C_NULL_FUNPTR
        !> Combination of METH_xxx flags, which mostly describe the args expected by the C func
        integer(kind=C_INT) :: ml_flags = 0
        !> The __doc__ attribute, or NULL
        type(C_PTR) :: ml_doc           = C_NULL_PTR
    end type PyMethodDef




    contains



end module ModuleSupport
