!############################################################################################
!     ______      ____       
!    / ____/_  __/ __ \__  __
!   / /_  / / / / /_/ / / / /
!  / __/ / /_/ / ____/ /_/ / 
! /_/    \__, /_/    \__, /  
!       /____/      /____/   
!
!  FyPy: Fortran Library for Python Extending and Imbedding
!
!> \brief       FyPy module
!! \details     It provides access to the Python/C API as well as helper routines for 
!!              implementing a Python extension module using pure Fortran code. Additional
!!              APTs are also provided for imbedding Python in a Fortran application. 
!!              The existing functionality incoludes addition of Python objects and methods
!!              for implementinf a Python extension module, conversion of Python data types
!!              (ints, floats, complex numbers, strings) to the equivalent Fortran/C types,
!!              handling of Python container objects (Tuples, Lists, Dictionaries, Sequences),
!!              importing Python modules, and conversion of Fortran arrays (up to rank 4) to
!!              NumPy arrays and vice-versa.
!! \author      Bharat Mahajan
!! \date        Created: 08/07/2019    
!
!############################################################################################



module FyPy

    use, intrinsic :: iso_c_binding, only: C_CHAR, C_NULL_CHAR, C_INT, C_LONG, C_LONG_LONG, &
                                        C_DOUBLE, C_PTR, C_FUNPTR, C_NULL_PTR, C_NULL_FUNPTR,  &
                                        c_associated, c_loc, c_f_pointer, c_funloc, &
                                        C_SIGNED_CHAR, C_INT8_T, C_INT32_T, C_INT64_T, &
                                        C_FLOAT, C_DOUBLE, C_DOUBLE_COMPLEX

    use Object
    use Numerics
    use Containers
    use ModuleSupport
    use Exceptions
    use FMUTIL, only: List
    
    implicit none

    public

    !> Python API version that is supported, see modsupport.h
    integer(kind=C_INT), parameter :: PYTHON_API_VERSION = 1013_C_INT

    !> Cache the python object types to be used for exact type testing
    type(C_PTR), private, save :: Py_None_Object = C_NULL_PTR !< Py_None object type

    type(C_PTR), private, save :: Py_True_Object = C_NULL_PTR !< Py_True object type
    type(C_PTR), private, save :: Py_False_Object = C_NULL_PTR !< Py_False object type

    type(C_PTR), private, save :: Py_Long_TypeObject = C_NULL_PTR !< Py_Long object type    
    type(C_PTR), private, save :: Py_Bool_TypeObject = C_NULL_PTR !< Py_Bool object type
    type(C_PTR), private, save :: Py_Float_TypeObject = C_NULL_PTR !< Py_Float object type
    type(C_PTR), private, save :: Py_Complex_TypeObject = C_NULL_PTR !< Py_Complex object type
    type(C_PTR), private, save :: Py_Unicode_TypeObject = C_NULL_PTR !< Py_Unicode object type
    
    type(C_PTR), private, save :: Py_Tuple_TypeObject = C_NULL_PTR !< Py_Tuple object type
    type(C_PTR), private, save :: Py_List_TypeObject = C_NULL_PTR !< Py_List object type
    type(C_PTR), private, save :: Py_Dict_TypeObject = C_NULL_PTR !< Py_Dictionary object type

    type(C_PTR), private, save :: Py_Array_TypeObject   = C_NULL_PTR !< NumPy Array object type
    

    !> Supported Error Codes
    enum, bind(C)
        enumerator :: FYPY_SUCCESS                 = 0   !< Successfully completed
        enumerator :: FYPY_ERROR                         !< Some unknown error
        enumerator :: FYPY_ERROR_PYTHONEXCEPTION         !< Python exception has occurredd
        enumerator :: FYPY_ERROR_PARAMS                  !< Wrong parameters
        enumerator :: FYPY_ERROR_ARGPARSING              !< Argument Parsing failed
        enumerator :: FYPY_ERROR_PYCALL                  !< Python object call failed
        enumerator :: FYPY_ERROR_PYMODULE_NOTFOUND       !< Python module is not found
        enumerator :: FYPY_ERROR_PYMODULE_IMPORTFAILED   !< Python module import failed        
        enumerator :: FYPY_ERROR_BUILTIN_NOT_FOUND       !< Python builtin object not found                
        enumerator :: FYPY_ERROR_NUMPY_NOT_FOUND         !< NumPy not found                        
        enumerator :: FYPY_ERROR_NUMPY_BUILTIN_ERROR     !< Numpy module builtin is not found
        enumerator :: FYPY_ERROR_NUMPY_ARRAY_DTYPE       !< Numpy array type error
        enumerator :: FYPY_ERROR_NUMPY_ARRAY_SHAPE       !< Numpy array shape error
        enumerator :: FYPY_ERROR_NUMPY_BUFFER_ERROR      !< Numpy buffer error                
        enumerator :: FYPY_ERROR_NUMPY_BUFFER_FORMAT     !< Numpy buffer format error
        enumerator :: FYPY_ERROR_NUMPY_NOT_CONTIGUOUS    !< Numpy array not contigous                        
    end enum   


    !> Supported Python Intrinsic Object Types
    enum, bind(C)
        enumerator :: FYPY_UNKNOWN           = 0   !< Some unknown type
        enumerator :: FYPY_LONG                    !< Long int type
        enumerator :: FYPY_LONG_LONG_INT           !< Long long int type
        enumerator :: FYPY_DOUBLE                  !< Double type
        enumerator :: FYPY_COMPLEX_DOUBLE          !< Double complex type
        enumerator :: FYPY_UNICODE_STRING          !< Unicode type
        enumerator :: FYPY_BOOL                    !< Boolean type
        enumerator :: FYPY_BYTES                   !< Bytes type
        enumerator :: FYPY_TUPLE                   !< Python tuple type
        enumerator :: FYPY_LIST                    !< Python list type
        enumerator :: FYPY_ARRAY                   !< Python array type
        enumerator :: FYPY_DICT                    !< Python Dict type
        enumerator :: FYPY_NUMPY_ARRAY             !< Numpy array type
    end enum   

    !> Module Methods calling conventions
    enum, bind(C)
        enumerator :: FYPY_METHODCALL_NOARGS           = 0   !< Some unknown type
        enumerator :: FYPY_METHODCALL_VARARGS                !< 
        enumerator :: FYPY_METHODCALL_VARGS_KEYWORDS         !< 
        enumerator :: FYPY_METHODCALL_FAST                   !< 
        enumerator :: FYPY_METHODCALL_FAST_KEYWORDS          !< 
        enumerator :: FYPY_METHODCALL_O                      !< 
    end enum
    
    !> NumPy data types supported for arrays
    enum, bind(C)
        enumerator :: FYPY_NUMPY_DTYPE_UNKNOWN         = 0
        enumerator :: FYPY_NUMPY_DTYPE_INT8         
        enumerator :: FYPY_NUMPY_DTYPE_INT32        
        enumerator :: FYPY_NUMPY_DTYPE_INT64         
        enumerator :: FYPY_NUMPY_DTYPE_FLOAT32       
        enumerator :: FYPY_NUMPY_DTYPE_FLOAT64       
        enumerator :: FYPY_NUMPY_DTYPE_Complex128
    end enum

    type :: NumPyClass  
        
        integer(kind(FYPY_SUCCESS)) :: status

        !> NumPy module
        type(C_PTR) :: NumPyModule                  = C_NULL_PTR

        !> NumPy module's ndarray method
        type(C_PTR) :: ndarray                      = C_NULL_PTR   

        ! data types
        type(C_PTR), private :: NUMPY_DTYPE_INT8      = C_NULL_PTR !< NumPy Array object type
        type(C_PTR), private :: NUMPY_DTYPE_INT32     = C_NULL_PTR !< NumPy Array object type
        type(C_PTR), private :: NUMPY_DTYPE_INT64     = C_NULL_PTR !< NumPy Array object type
        type(C_PTR), private :: NUMPY_DTYPE_FLOAT32   = C_NULL_PTR !< NumPy Array object type
        type(C_PTR), private :: NUMPY_DTYPE_FLOAT64   = C_NULL_PTR !< NumPy Array object type
        type(C_PTR), private :: NUMPY_DTYPE_COMPLEX128 = C_NULL_PTR !< NumPy Array object type
        
        contains

        !> NumPy Init method
        procedure, public :: Init => NumPyInit

        !> NumPy destructor for freeing memory
        final :: NumPyDestroy

    end type NumPyClass



    type :: FyPyClass
      
        private

        !> status of the last operation
        integer(kind(FYPY_SUCCESS)), public :: status           = FYPY_SUCCESS

        !> Pointer to dictionary of Pythons Builtins
        type(C_PTR) :: PyBuiltins                              = C_NULL_PTR
        
        !> Pointer to python print function
        type(C_PTR) :: PyPrint                                  = C_NULL_PTR

        !> Pointer to the python str function
        type(C_PTR) :: str                                      = C_NULL_PTR

        !> Total number of methods exported by this module
        integer :: ExportedMethods                              = 0

        !> Total number of methods exported by this module
        integer :: ExportedObjects                              = 0

        !> Pointer to the allocated module
        type(PyModuleDef), pointer :: pModule                   => null()

        !> Store the module object
        type(PyObject), pointer :: pModuleObj                   => null()

        !> Pointer to the allocated methods
        type(PyMethodDef), dimension(:), pointer :: pMethods    => null()

        !> NumPy class
        type(NumPyClass), public :: NumPy
        
    contains
        
        private

        !> FyPy Initialization, must be done before FyPySetModuleMethod
        procedure, public :: Init => FyPyInit

        !> Use this method to add new python module-level methods
        procedure, public :: AddMethod => FyPyAddMethod

        !> Use this method to add new python module-level objects
        procedure, public :: AddObject => FyPyAddObject

        !> Use this method to create a new python module-level custom type object
        procedure, public :: CreateTypeObject => FyPyCreateTypeObject

        !> Use this method to create the python module. This should be the last thing to do
        !! in PyInit_<MODULE_NAME> procedure.
        procedure, public :: CreatePyModule => FyPyCreatePyModule

        !> Use this method to parse the input argument tuple
        procedure, public :: ParseTuple => FyPyParseTuple
        
        !> Copies a fortran array into a numpy array
        procedure, public :: Fy2PyArray

        !> Copies a python array into a fortran array
        procedure, public :: Py2FyArray

        !> clean up routine
        final :: FyPyDestroy

    end type FyPyClass
    

    !> Type to encapsulate a C array pointer
    type :: CPtr
        type(C_PTR) :: ptr = C_NULL_PTR
    end type CPtr

    !> Method Argument Format Type used to parse Python input args tuple
    type, public :: FyPyMethodArgsFormat
        !> Type of the Python object expected
        integer(kind(FYPY_LONG))                :: PyObjType = FYPY_DOUBLE
        !> Flag for indicating whether this parameter is optional
        logical                                 :: optional = .FALSE.
        !> Keyword name to use for keyword args
        character(kind=C_CHAR,len=:), allocatable :: kw
        !> Is the expected parameter is a Numpy array
        logical                                 :: IsNumpyArr = .FALSE.
        !> Data type of the Numpy array expected
        integer(kind(FYPY_NUMPY_DTYPE_UNKNOWN))    :: NumpyDType = FYPY_NUMPY_DTYPE_FLOAT32
        !> Numpy array rank expected
        integer                                 :: NumpyArrRank = 0
        !> Extent of the expected Numpy array 
        !! [a,b] means any numpy array with rank = NumpyArrRank with its 
        !! first dimension = a and 2nd dimension = b is a valid argument
        integer, dimension(:), allocatable      :: NumpyArrExtent
    end type FyPyMethodArgsFormat





    interface
    
    module subroutine NumPyInit(me)
        class(NumPyClass), intent(inout) :: me
    end subroutine NumPyInit    

    module subroutine NumPyDestroy(me)
        type(NumPyClass), intent(inout) :: me
    end subroutine NumPyDestroy
    
    
    module subroutine FyPyInit(me, Num_Objects, Num_Methods, Load_NumPy)
        implicit none
        class(FyPyClass), intent(inout) :: me
        integer, intent(in) :: Num_Methods, Num_Objects
        logical :: Load_NumPy
    end subroutine FyPyInit

    module subroutine FyPyAddMethod(me, name, docstr, &
                NoArgsMethod, ArgsMethod, ArgsKwMethod, FastMethod, FastKwMethod)
        implicit none
        class(FyPyClass), intent(inout) :: me
        character(kind=C_CHAR,len=*), intent(in) :: name
        character(kind=C_CHAR,len=*), intent(in) :: docstr
        procedure(), optional           :: NoArgsMethod
        procedure(), optional                :: ArgsMethod
        procedure(), optional    :: ArgsKwMethod
        procedure(), optional            :: FastMethod
        procedure(), optional :: FastKwMethod
    end subroutine FyPyAddMethod

    module subroutine FyPyAddObject(me, ModuleObj, name, obj)
        implicit none
        class(FyPyClass), intent(inout) :: me
        type(C_PTR), value :: ModuleObj
        character(kind=C_CHAR, len=*), intent(in) :: name
        type(C_PTR), value :: obj
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
    end subroutine FyPyCreateTypeObject


    type(C_PTR) module function FyPyCreatePyModule(me, name, docstr)
        implicit none
        class(FyPyClass), intent(inout) :: me
        character(kind=C_CHAR,len=*), intent(in) :: name
        character(kind=C_CHAR,len=*), intent(in) :: docstr
    end function FyPyCreatePyModule


    module subroutine FyPyParseTuple(me, fmt, PyTuple, ErrStr, status, ArgsList)
        implicit none
        class(FyPyClass), intent(inout) :: me
        type(FyPyMethodArgsFormat), dimension(:), intent(inout) :: fmt
        type(C_PTR), value :: PyTuple
        character(len=*), intent(in) :: ErrStr
        integer(kind(FYPY_SUCCESS)), intent(out) :: status
        type(List), intent(out)     :: ArgsList
    end subroutine FyPyParseTuple



    integer(kind(FYPY_SUCCESS)) module function Fy2PyArray(me, arr, arrtype, arrshape, pyarr)
        class(FyPyclass), intent(inout) :: me
        type(C_PTR), intent(in) :: arr
        integer(kind(FYPY_NUMPY_DTYPE_BOOL)), intent(in) :: arrtype
        integer, dimension(:), intent(in) :: arrshape
        type(C_PTR), intent(out) :: pyarr
    end function Fy2PyArray 
    
    integer(kind(FYPY_SUCCESS)) module function Py2FyArray(me, pyarr, arrtype, arrshape, arr)
        implicit none
        class(FyPyclass), intent(inout) :: me
        type(C_PTR), intent(in) :: pyarr
        integer(kind(FYPY_NUMPY_DTYPE_INT8)), intent(out) :: arrtype
        integer, dimension(:), allocatable, intent(out) :: arrshape
        type(C_PTR), intent(out) :: arr
    end function Py2FyArray


    module subroutine FyPyDestroy(me)
        type(FyPyClass), intent(inout) :: me
    end subroutine FyPyDestroy

    end interface

    
    
    interface
    
    !> FyPy print function for an array of Python Objects    
    module subroutine FyPyPrintObjects(objs)
        implicit none
        type(C_PTR), dimension(:), intent(in) :: objs
        !integer(kind(FYPY_SUCCESS)), intent(out), optional :: status        
    end subroutine FyPyPrintObjects

    !> FyPy print function for fortran string
    module subroutine FyPyPrintStr(str)
        implicit none
        character(len=*), intent(in) :: str
        !integer(kind(FYPY_SUCCESS)), intent(inout), optional :: status        
    end subroutine FyPyPrintStr
    
    end interface

    
    
    interface FyPyPrint
        module procedure :: FyPyPrintObjects
        module procedure :: FyPyPrintStr
    end interface

    
    
    interface

    !> Use this procedure to load or reload any python module or its attributes
    module subroutine FyPyImportModule(status, modobj, modname, modpath, fromlist, globaldict, localdict, Reload)
        implicit none
        integer(kind(FYPY_SUCCESS)), intent(out) :: status
        type(C_PTR), intent(out) :: modobj
        character(len=*), intent(in) :: modname
        character(len=*), intent(in), optional :: modpath
        character(len=100), dimension(:), intent(in), optional :: fromlist
        type(C_PTR), value, optional :: globaldict
        type(C_PTR), value, optional :: localdict
        logical, intent(in), optional :: Reload
    end subroutine FyPyImportModule


    module subroutine FyPyCallPyObject(PyFunObj, ret, args, kwargs)
        implicit none
        type(C_PTR), value :: PyFunObj !< Python callback function object
        type(C_PTR), intent(out) :: ret
        type(C_PTR), value, optional :: args
        type(C_PTR), value, optional :: kwargs
    end subroutine FyPyCallPyObject

    !> Determines whether the python object is of Long type or subtype
    logical module function IsPyLongType(obj, CheckExactType)
        type(C_PTR), value :: obj
        logical, intent(in), optional :: CheckExactType
    end function IsPyLongType

    !> Determines whether the python object is of float type or subtype  
    logical module function IsPyFloatType(obj, CheckExactType)
        type(C_PTR), value :: obj
        logical, intent(in), optional :: CheckExactType
    end function IsPyFloatType

    !> Determines whether the python object is of complex type or subtype
    logical module function IsPyComplexType(obj, CheckExactType)
        implicit none
        type(C_PTR), value :: obj
        logical, intent(in), optional :: CheckExactType
    end function IsPyComplexType

    !> Determines whether the python object is of bool type
    logical module function IsPyBoolType(obj)
        implicit none
        type(C_PTR), value :: obj
    end function IsPyBoolType

    !> Determines whether the python object is of unicode type or subtype
    logical module function IsPyUnicodeType(obj, CheckExactType)
        implicit none
        type(C_PTR), value :: obj
        logical, intent(in), optional :: CheckExactType
    end function IsPyUnicodeType

    !> see tupleobject.h for PyTuple_Check and PyTuple_CheckExact
    logical module function IsPyTupleType(obj, CheckExactType)
        implicit none
        type(C_PTR), value :: obj
        logical, intent(in), optional :: CheckExactType
    end function IsPyTupleType

    !> see tupleobject.h for PyTuple_Check and PyTuple_CheckExact
    logical module function IsPyListType(obj, CheckExactType)
        implicit none
        type(C_PTR), value :: obj
        logical, intent(in), optional :: CheckExactType
    end function IsPyListType
    
    !> see dictobject.h for PyDict_Check and PyDict_CheckExact
    logical module function IsPyDictType(obj, CheckExactType)
        implicit none
        type(C_PTR), value :: obj
        logical, intent(in), optional :: CheckExactType
    end function IsPyDictType
    
    logical module function IsPyArrayType(obj, CheckExactType)
        implicit none
        type(C_PTR), value :: obj
        logical, intent(in), optional :: CheckExactType
    end function IsPyArrayType

    end interface




    !> Common interface to convert Fortran scalar data types to Python object types
    interface Fy2PyType
        module procedure :: Fy2PyLong
        module procedure :: Fy2PyLongLong
        module procedure :: Fy2PyDouble
        module procedure :: Fy2PyBool
        module procedure :: Fy2PyComplex
        module procedure :: Fy2PyString
    end interface

    !> Common interface to convert Python object types to Fortran data types
    interface Py2FyType
        module procedure :: Py2FyLong
        module procedure :: Py2FyLongLong
        module procedure :: Py2FyDouble
        module procedure :: Py2FyBool
        module procedure :: Py2FyComplex
        module procedure :: Py2FyString
    end interface


    interface

    !> Note long int can be equivalent to kind=4 or kind=8 in ifort 
    !! depending on the 32-bit or 64-bit mode for target architecture
    integer(kind(FYPY_SUCCESS)) module function Fy2PyLong(longintvar, obj)
        integer(C_LONG), intent(in) :: longintvar
        type(C_PTR), intent(out) :: obj
    end function Fy2PyLong

    integer(kind(FYPY_SUCCESS)) module function Fy2PyLongLong(longlongint, obj)
        integer(C_LONG_LONG), intent(in) :: longlongint
        type(C_PTR), intent(out) :: obj
    end function Fy2PyLongLong

    integer(kind(FYPY_SUCCESS)) module function Py2FyLong(obj, longintvar)
        type(C_PTR), value :: obj
        integer(C_LONG), intent(out) :: longintvar
    end function Py2FyLong

    integer(kind(FYPY_SUCCESS)) module function Py2FyLongLong(obj, longlongint)
        type(C_PTR), value :: obj
        integer(C_LONG_LONG), intent(out) :: longlongint
    end function Py2FyLongLong

    integer(kind(FYPY_SUCCESS)) module function Fy2PyDouble(dblvar, obj)
        real(kind=C_Double), value :: dblvar
        type(C_PTR), intent(out) :: obj
    end function Fy2PyDouble

    integer(kind(FYPY_SUCCESS)) module function Py2FyDouble(obj, dblvar)
        type(C_PTR), value :: obj
        real(kind=C_DOUBLE), intent(out) :: dblvar
    end function Py2FyDouble

    integer(kind(FYPY_SUCCESS)) module function Fy2PyBool(boolvar, obj)
        logical, intent(in) :: boolvar
        type(C_PTR), intent(out) :: obj
    end function Fy2PyBool

    integer(kind(FYPY_SUCCESS)) module function Py2FyBool(obj, boolvar)
        logical, intent(out) :: boolvar
        type(C_PTR), value :: obj
    end function Py2FyBool

    integer(kind(FYPY_SUCCESS)) module function Fy2PyComplex(cmplxvar, obj)
        complex(C_DOUBLE), intent(in) :: cmplxvar
        type(C_PTR), intent(out) :: obj
    end function Fy2PyComplex

    integer(kind(FYPY_SUCCESS)) module function Py2FyComplex(obj, cmplxvar)
        type(C_PTR), value :: obj
        complex(C_DOUBLE), intent(out) :: cmplxvar
    end function Py2FyComplex

    integer(kind(FYPY_SUCCESS)) module function Fy2PyString(strvar, obj)
        character(kind=C_CHAR, len=*), intent(in) :: strvar
        type(C_PTR), intent(out) :: obj
    end function Fy2PyString

    integer(kind(FYPY_SUCCESS)) module function Py2FyString(obj, strvar)
        type(C_PTR), value :: obj
        character(len=:), allocatable, intent(out) :: strvar
    end function Py2FyString

    end interface
    
    contains
    
    
    

end module FyPy



