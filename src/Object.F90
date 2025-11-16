!############################################################################################
!
!  FyPy
!
!> \brief       Object module
!! \details     It provides Fortran equivalent of Python Object related APIs of CPython
!! \author      Bharat Mahajan
!! \date        08/07/2019    
!
!############################################################################################


#include "FyPyMacros.fi"

! Additional Macros

! PyObject_HEAD defines the initial segment of every PyObject
#define PyObject_HEAD                   type(PyObject) :: ob_base

! PyObject_VAR_HEAD defines the initial segment of all variable-size
! container objects.  These end with a declaration of an array with 1
! element, but enough space is malloc'ed so that the array actually
! has room for ob_size elements.  Note that ob_size is an element count,
! not necessarily a byte count.
#define PyObject_VAR_HEAD      type(PyVarObject) :: ob_base



module Object

    use, intrinsic :: iso_c_binding, only: C_INTPTR_T, C_INT, C_LONG, C_PTR, C_FUNPTR,&
                        C_NULL_PTR, C_NULL_FUNPTR, c_associated, C_CHAR,C_SIGNED_CHAR
                                            

    implicit none

    public

    !> Py_ssize_t must be unsigned integral type. See CPython's pyport.h
    integer, parameter, public :: PY_SSIZE_T = C_INTPTR_T

#ifdef Py_REF_DEBUG
    !> This is for debug ref counting
    integer(kind=PY_SSIZE_T), bind(C, name="_Py_RefTotal") :: Py_RefTotal
#endif

    !> Type Flags (tp_flags), see object.h
    integer(C_LONG), parameter, public :: Py_TPFLAGS_HEAPTYPE      = lshift(1, 9)
    integer(C_LONG), parameter, public :: Py_TPFLAGS_BASETYPE      = lshift(1, 10)
    integer(C_LONG), parameter, public :: Py_TPFLAGS_READY         = lshift(1, 12)
    integer(C_LONG), parameter, public :: Py_TPFLAGS_READYING      = lshift(1, 13)
    integer(C_LONG), parameter, public :: Py_TPFLAGS_HAVE_GC       = lshift(1, 14)

#ifdef STACKLESS
    integer(C_LONG), parameter, public :: Py_TPFLAGS_HAVE_STACKLESS_EXTENSION = lshift(3, 15)
#else
    integer(C_LONG), parameter, public :: Py_TPFLAGS_HAVE_STACKLESS_EXTENSION = 0
#endif
   
    integer(C_LONG), parameter, public :: Py_TPFLAGS_HAVE_VERSION_TAG  = lshift(1, 18)
    integer(C_LONG), parameter, public :: Py_TPFLAGS_VALID_VERSION_TAG = lshift(1, 19)
    integer(C_LONG), parameter, public :: Py_TPFLAGS_IS_ABSTRACT = lshift(1, 20)

    !> Python/C flags to be used to determine the subclass object type, see object.h
    integer(C_LONG), parameter, public :: Py_TPFLAGS_LONG_SUBCLASS      = lshift(1, 24)
    integer(C_LONG), parameter, public :: Py_TPFLAGS_LIST_SUBCLASS      = lshift(1, 25)
    integer(C_LONG), parameter, public :: Py_TPFLAGS_TUPLE_SUBCLASS     = lshift(1, 26)
    integer(C_LONG), parameter, public :: Py_TPFLAGS_BYTES_SUBCLASS     = lshift(1, 27)
    integer(C_LONG), parameter, public :: Py_TPFLAGS_UNICODE_SUBCLASS   = lshift(1, 28)
    integer(C_LONG), parameter, public :: Py_TPFLAGS_DICT_SUBCLASS      = lshift(1, 29)
    integer(C_LONG), parameter, public :: Py_TPFLAGS_BASE_EXC_SUBCLASS  = lshift(1, 30)
    integer(C_LONG), parameter, public :: Py_TPFLAGS_TYPE_SUBCLASS      = lshift(1, 31)

    integer(C_LONG), parameter, public :: Py_TPFLAGS_DEFAULT = ior(0,ior(Py_TPFLAGS_HAVE_VERSION_TAG,&
                                                    Py_TPFLAGS_HAVE_STACKLESS_EXTENSION))


    !> Structure to hold pointers to the functions required for implementing 
    !! awaitable and asynchronous iterator objects, see CPython's object.h
    type, bind(C) :: PyAsyncMethods
        type(C_FUNPTR) :: am_await = C_NULL_FUNPTR
        type(C_FUNPTR) :: am_aiter = C_NULL_FUNPTR
        type(C_FUNPTR) :: am_anext = C_NULL_FUNPTR
    end type PyAsyncMethods

    !> PyObject: see CPython's object.h
    type, public, bind(C) :: PyObject
#ifdef Py_TRACE_REFS
        ! Define pointers to support a doubly-linked list of all live heap objects
        type(C_PTR) :: ob_next                  = C_NULL_PTR
        type(C_PTR) :: ob_prev                  = C_NULL_PTR
#endif
        integer(kind=PY_SSIZE_T) :: ob_refcnt   = 0
        type(C_PTR) :: ob_type                  = C_NULL_PTR
    end type PyObject

    !> PyVarObject: see CPython's object.h
    type, bind(C) :: PyVarObject
        type(PyObject) :: ob_base !< This is basically the PyObject_HEAD CPython macro
        integer(kind=PY_SSIZE_T) :: ob_size = 0 ! Number of items in variable part
    end type PyVarObject

    !> Initialization of the PyTypeObject header, see CPython's object.h
    type(PyVarObject), parameter :: PyVarObject_HEAD_INIT = PyVarObject(&
                                                                PyObject(&
#ifdef Py_TRACE_REFS
                                                                    ob_next=C_NULL_PTR, &
                                                                    ob_prev=C_NULL_PTR, &
#endif                                                                    
                                                                    ob_refcnt = 1, &
                                                                    ob_type = C_NULL_PTR), &
                                                                ob_size = 0)


    !> PyTypeObject: see CPython's object.h
    !! FORPEX does not use CPython's Limited API
    type, bind(C) :: PyTypeObject
        
        PyObject_VAR_HEAD

        !> For printing, in format "<module>.<name>" 
        type(C_PTR) :: tp_name                      = C_NULL_PTR
        
        integer(kind=PY_SSIZE_T) :: tp_basicsize    = 0 ! For allocation
        integer(kind=PY_SSIZE_T) :: tp_itemsize     = 0 ! For allocation
    
        ! Methods to implement standard operations
    
        type(C_FUNPTR) :: tp_dealloc                = C_NULL_FUNPTR
        type(C_FUNPTR) :: tp_print                  = C_NULL_FUNPTR
        type(C_FUNPTR) :: tp_getattr                = C_NULL_FUNPTR
        type(C_FUNPTR) :: tp_setattr                = C_NULL_FUNPTR
        !> formerly known as tp_compare (Python 2) or tp_reserved (Python 3)
        type(C_PTR) :: tp_as_async                  = C_NULL_PTR
        type(C_FUNPTR) :: tp_repr                   = C_NULL_FUNPTR
    
        ! Method suites for standard classes
    
        type(C_PTR) :: tp_as_number                 = C_NULL_PTR
        type(C_PTR) :: tp_as_sequence               = C_NULL_PTR
        type(C_PTR) :: tp_as_mapping                = C_NULL_PTR
    
        ! More standard operations (here for binary compatibility)
    
        type(C_FUNPTR) :: tp_hash                   = C_NULL_FUNPTR
        type(C_FUNPTR) :: tp_call                   = C_NULL_FUNPTR
        type(C_FUNPTR) :: tp_str                    = C_NULL_FUNPTR
        type(C_FUNPTR) :: tp_getattro               = C_NULL_FUNPTR
        type(C_FUNPTR) :: tp_setattro               = C_NULL_FUNPTR
    
        !> Functions to access object as input/output buffer
        type(C_PTR) :: tp_as_buffer                 = C_NULL_PTR
    
        !> Flags to define presence of optional/expanded features
        integer(kind=C_LONG) :: tp_flags            = 0
    
        type(C_PTR) :: tp_doc                       = C_NULL_PTR! Documentation string
    
        !> Assigned meaning in release 2.0
        !! call function for all accessible objects
        type(C_FUNPTR) :: tp_traverse               = C_NULL_FUNPTR
    
        !> delete references to contained objects
        type(C_FUNPTR) :: tp_clear                  = C_NULL_FUNPTR
    
        !> Assigned meaning in release 2.1 
        !! rich comparisons 
        type(C_FUNPTR) :: tp_richcompare            = C_NULL_FUNPTR
    
        ! weak reference enabler 
     
        integer(kind=PY_SSIZE_T) :: tp_weaklistoffset = 0
    
        ! Iterators
     
        type(C_FUNPTR) :: tp_iter                   = C_NULL_FUNPTR
        type(C_FUNPTR) :: tp_iternext               = C_NULL_FUNPTR
    
        ! Attribute descriptor and subclassing stuff
     
        type(C_PTR) :: tp_methods                   = C_NULL_PTR
        type(C_PTR) :: tp_members                   = C_NULL_PTR
        type(C_PTR) :: tp_getset                    = C_NULL_PTR
        type(C_PTR) :: tp_base                      = C_NULL_PTR
        type(C_PTR) :: tp_dict                      = C_NULL_PTR
        type(C_FUNPTR) :: tp_descr_get              = C_NULL_FUNPTR
        type(C_FUNPTR) :: tp_descr_set              = C_NULL_FUNPTR
        integer(kind=PY_SSIZE_T) :: tp_dictoffset   = 0
        type(C_FUNPTR) :: tp_init                   = C_NULL_FUNPTR 
        type(C_FUNPTR) :: tp_alloc                  = C_NULL_FUNPTR
        type(C_FUNPTR) :: tp_new                    = C_NULL_FUNPTR
        type(C_FUNPTR) :: tp_free                   = C_NULL_FUNPTR !< Low-level free-memory routine
        type(C_FUNPTR) :: tp_is_gc                  = C_NULL_FUNPTR !< For PyObject_IS_GC
        type(C_PTR) :: tp_bases                     = C_NULL_PTR
        type(C_PTR) :: tp_mro                       = C_NULL_PTR !< method resolution order
        type(C_PTR) :: tp_cache                     = C_NULL_PTR        
        type(C_PTR) :: tp_subclasses                = C_NULL_PTR
        type(C_PTR) :: tp_weaklist                  = C_NULL_PTR
        type(C_FUNPTR) :: tp_del                    = C_NULL_FUNPTR
    
        !> Type attribute cache version tag. Added in version 2.6
        integer(kind=C_INT) :: tp_version_tag       = 0
    
        type(C_FUNPTR) :: tp_finalize               = C_NULL_FUNPTR
    
#ifdef COUNT_ALLOCS
        ! these must be last and never explicitly initialized
        integer(kind=PY_SSIZE_T) :: tp_allocs       = 0
        integer(kind=PY_SSIZE_T) :: tp_frees        = 0
        integer(kind=PY_SSIZE_T) :: tp_maxalloc     = 0
        type(C_PTR) :: tp_prev                      = C_NULL_PTR
        type(C_PTR) :: tp_next                      = C_NULL_PTR
#endif

    end type PyTypeObject

#ifndef Py_LIMITED_API

!> Flags for getting buffers (see object.h in CPython)

    integer(C_INT), parameter :: PyBUF_SIMPLE = 0
    integer(C_INT), parameter :: PyBUF_WRITABLE = int(z'0001')

    !> for backward compatibility
    integer(C_INT), parameter :: PyBUF_WRITEABLE = PyBUF_WRITABLE

    integer(C_INT), parameter :: PyBUF_FORMAT = int(z'0004')
    integer(C_INT), parameter :: PyBUF_ND = int(z'0008')
    integer(C_INT), parameter :: PyBUF_STRIDES = ior(int(z'0010'), PyBUF_ND)
    integer(C_INT), parameter :: PyBUF_C_CONTIGUOUS = ior(int(z'0020'), PyBUF_STRIDES)
    integer(C_INT), parameter :: PyBUF_F_CONTIGUOUS = ior(int(z'0040'), PyBUF_STRIDES)
    integer(C_INT), parameter :: PyBUF_ANY_CONTIGUOUS = ior(int(z'0080'), PyBUF_STRIDES)
    integer(C_INT), parameter :: PyBUF_INDIRECT = ior(int(z'0100'), PyBUF_STRIDES)

    integer(C_INT), parameter :: PyBUF_CONTIG = ior(PyBUF_ND, PyBUF_WRITABLE)
    integer(C_INT), parameter :: PyBUF_CONTIG_RO = PyBUF_ND

    integer(C_INT), parameter :: PyBUF_STRIDED = ior(PyBUF_STRIDES, PyBUF_WRITABLE)
    integer(C_INT), parameter :: PyBUF_STRIDED_RO = PyBUF_STRIDES

    integer(C_INT), parameter :: PyBUF_RECORDS = ior(PyBUF_STRIDES, ior(PyBUF_WRITABLE, PyBUF_FORMAT))
    integer(C_INT), parameter :: PyBUF_RECORDS_RO = ior(PyBUF_STRIDES, PyBUF_FORMAT)

    integer(C_INT), parameter :: PyBUF_FULL = ior(PyBUF_INDIRECT, ior(PyBUF_WRITABLE, PyBUF_FORMAT))
    integer(C_INT), parameter :: PyBUF_FULL_RO = ior(PyBUF_INDIRECT, PyBUF_FORMAT)

    integer(C_INT), parameter :: PyBUF_READ  = int(z'100')
    integer(C_INT), parameter :: PyBUF_WRITE = int(z'200')


    !> Python Buffer structure (not a python object)
    type, bind(C) :: Py_Buffer
        type(C_PTR)         :: buf      = C_NULL_PTR
        type(C_PTR)         :: obj      = C_NULL_PTR
        integer(PY_SSIZE_T) :: length      = -1
        integer(PY_SSIZE_T) :: itemsize = -1
        integer(C_INT)      :: readonly = -1
        integer(C_INT)      :: ndim     = -1
        type(C_PTR)         :: formatstr   = C_NULL_PTR
        type(C_PTR)         :: shape    = C_NULL_PTR
        type(C_PTR)         :: strides  = C_NULL_PTR
        type(C_PTR)         :: suboffsets = C_NULL_PTR
        type(C_PTR)         :: internal = C_NULL_PTR                        
    end type Py_Buffer

#endif


    interface

    !#####################
    ! Reference management
    !#####################


    !> Increments ref counter, see Py_IncRef macro in CPython's object.h
    subroutine Py_IncRef(obj) bind(C, name='Py_IncRef')
        import :: C_PTR
        implicit none
        type(C_PTR), value :: obj
    end subroutine Py_IncRef

    !> Deccrements ref counter and calls object's deallocator if refctr=0
    !! see Py_IncRef macro in CPython's object.h
    subroutine Py_DecRef(obj) bind(C, name='Py_DecRef')
        import :: C_PTR
        implicit none
        type(C_PTR), value :: obj
    end subroutine Py_DecRef


    !####################
    ! Type Object Related
    !####################


    !> Determines whether given type object a is a subtype of type object b
    integer(C_INT) function PyType_IsSubtype(obtype_a, obtype_b) bind(C, name="PyType_IsSubtype")
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: obtype_a
        type(C_PTR), value :: obtype_b
    end function PyType_IsSubtype


    !> Finalizes a type object initialization. Returns 0 on success or -1 on error
    !! with appropriate exception set 
    integer(C_INT) function PyType_Ready(type) bind(C, name='PyType_Ready')
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: type !< PyTypeObject type
    end function PyType_Ready

    !> generic handler for the tp_new slot of a type object. new reference.
    type(C_PTR) function PyType_GenericNew(type, args, kwds) bind(C, name='PyType_GenericNew')
        import :: C_PTR
        implicit none
        type(C_PTR), value :: type !< PyTypeObject
        type(C_PTR), value :: args !< argument tuple
        type(C_PTR), value :: kwds !< dictionary keywords
    end function PyType_GenericNew


    !#########################
    ! Abstract Object Protocol
    !#########################

    !> Determines if obj is callable. Return 1 if yes, otherwise 0.
    integer(C_INT) function PyCallable_Check(obj) bind(C, name='PyCallable_Check')
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: obj !< PyObject
    end function PyCallable_Check


    !> Call a Python object callable with arguments tuple args, and named arguments by dictionary
    !! kwargs. Returns NULL on failure along with raining exception. New reference.
    type(C_PTR) function PyObject_Call(callable, args, kwargs) bind(C, name='PyObject_Call')
        import :: C_PTR
        implicit none
        type(C_PTR), value :: callable !< PyObject
        type(C_PTR), value :: args !< argument tuple, can be empty tuple
        type(C_PTR), value :: kwargs !< keyword arguments, can be NULL
    end function PyObject_Call


    !> Call a Python object callable with arguments tuple args
    !! Returns NULL on failure along with raining exception. New reference.
    type(C_PTR) function PyObject_CallObject(callable, args) bind(C, name='PyObject_CallObject')
        import :: C_PTR
        implicit none
        type(C_PTR), value :: callable !< PyObject
        type(C_PTR), value :: args !< argument tuple, can be NULL
    end function PyObject_CallObject

    !> Return the attribute from object. NULL on failure. New reference
    type(C_PTR) function PyObject_GetAttr(obj, attr) bind(C, name='PyObject_GetAttr')
        import :: C_PTR
        implicit none
        type(C_PTR), value :: obj !< PyObject
        type(C_PTR), value :: attr !< argument tuple, can be NULL
    end function PyObject_GetAttr

    !> Return the attribute from object. NULL on failure. New reference
    type(C_PTR) function PyObject_GetAttrString(obj, attr) bind(C, name='PyObject_GetAttrString')
        import :: C_PTR, C_CHAR
        implicit none
        type(C_PTR), value :: obj !< PyObject
        character(kind=C_CHAR), dimension(*), intent(in) :: attr !< argument tuple, can be NULL
    end function PyObject_GetAttrString

    !> Set the attribute of object to v. 0 on success or -1 on failure.
    integer(C_INT) function PyObject_SetAttr(obj, attr, v) bind(C, name='PyObject_SetAttr')
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: obj !< PyObject
        type(C_PTR), value :: attr !< argument tuple, can be NULL
        type(C_PTR), value :: v !< argument tuple, can be NULL
    end function PyObject_SetAttr

    !> Set the attribute of object to v. 0 on success or -1 on failure.
    integer(C_INT) function PyObject_SetAttrString(obj, attr, v) bind(C, name='PyObject_SetAttrString')
        import :: C_PTR, C_INT, C_CHAR
        implicit none
        type(C_PTR), value :: obj !< PyObject
        character(kind=C_CHAR), dimension(*), intent(in) :: attr !< argument tuple, can be NULL
        type(C_PTR), value :: v !< argument tuple, can be NULL
    end function PyObject_SetAttrString


    !################
    ! Buffer Protocol
    !################

    !> Returns 1 if obj supports the buffer protocol, otherwise 0.
    integer(C_INT) function PyObject_CheckBuffer(obj) bind(C, name='PyObject_CheckBuffer')
        import :: C_INT, C_PTR
        implicit none
        type(C_PTR), value :: obj 
    end function PyObject_CheckBuffer


    !> Send a request to exporter to fill in view as soecified by flags. In case of any error,
    !! exporter raises PyExc_BufferError and set view to NULL and return -1, otherwise 0.
    !! Successful calls to  PyObject_GetBuffer() must be paired with  PyBuffer_Release(). 
    integer(C_INT) function PyObject_GetBuffer(exporter, view, flags) bind(C, name='PyObject_GetBuffer')
        import :: C_INT, C_PTR
        implicit none
        type(C_PTR), value :: exporter
        type(C_PTR), value :: view
        integer(C_INT), value :: flags
    end function PyObject_GetBuffer


    !> Releases the buffer view and decrement the reference counter view->obj.
    !! Must be called only on those buffers obtained by PyObject_GetBuffer.
    subroutine PyBuffer_Release(view) bind(C, name='PyBuffer_Release')
        import :: C_PTR
        implicit none
        type(C_PTR), value :: view 
    end subroutine PyBuffer_Release


    !> Returns 1 if memory defined by view is 'C'-style or 'F'ortran-style
    !! contiguous or either one (order is 'A'). Returns 0 otherwise.
    integer(C_INT) function PyBuffer_IsContiguous(view, order) bind(C, name='PyBuffer_IsContiguous')
        import :: C_INT, C_PTR, C_CHAR
        implicit none
        type(C_PTR), value :: view
        character(C_CHAR), value :: order
    end function PyBuffer_IsContiguous


    !> get the memory area pointed to by the indices inside the given view.
    type(C_PTR) function PyBuffer_GetPointer(view, indices) bind(C, name='PyBuffer_GetPointer')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: view
        integer(PY_SSIZE_T), value :: indices
    end function PyBuffer_GetPointer


    !> Copy len bytes from src to its contiguous representation in buf. Order can be 'C'
    !! or 'F' (C-style or Fortran-style). Returns 0 otherwise, -1 on succcess.
    integer(C_INT) function PyBuffer_ToContiguous(buf, src, length, order) bind(C, name='PyBuffer_ToContiguous')
        import :: C_INT, C_PTR, C_SIGNED_CHAR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: buf
        type(C_PTR), value :: src
        integer(PY_SSIZE_T), value :: length
        integer(C_SIGNED_CHAR), value :: order
    end function PyBuffer_ToContiguous

    !> Fill the strides array with byte-strides of a contiguous (C-style or F-style) array of the
    !! given shape with the given number of byter per element 
    subroutine PyBuffer_FillContiguousStrides(ndims, shape, strides, itemsize, order) bind(C,name='PyBuffer_FillContiguousStrides')
        import :: C_PTR, C_INT, C_SIGNED_CHAR
        implicit none
        integer(C_INT), value :: ndims
        type(C_PTR), value :: shape
        type(C_PTR), value :: strides
        integer(C_INT), value :: itemsize
        integer(C_SIGNED_CHAR), value :: order
    end subroutine PyBuffer_FillContiguousStrides


    !> Copy len bytes from src to its contiguous representation in buf. Order can be 'C'
    !! or 'F' (C-style or Fortran-style). Returns 0 otherwise, -1 on succcess.
    integer(C_INT) function PyBuffer_FillInfo(view, exporter,buf, length, readonly,flags) bind(C, name='PyBuffer_FillInfo')
        import :: C_INT, C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: view
        type(C_PTR), value :: exporter
        type(C_PTR), value :: buf
        integer(PY_SSIZE_T), value :: length
        integer(C_INT), value :: readonly
        integer(C_INT), value :: flags
    end function PyBuffer_FillInfo


    end interface






    contains



    !#################################################################
    ! Following procedures are direct conversion of CPython API macros
    ! (Don't worry fortran compiler will inline these as it sees fit)
    !#################################################################


    !> Increments ref counter, see Py_IncRef macro in CPython's object.h
    !! object may be NULL, in which case it has no effect
    subroutine Py_XIncRef(obj)
        implicit none
        type(C_PTR), value :: obj
        type(C_PTR) :: tmpobj

        tmpobj = obj
        if (c_associated(tmpobj)) call Py_IncRef(tmpobj)
    end subroutine Py_XIncRef


    !> Decrements ref counter and calls object's deallocator if refctr=0
    !! object may be NULL, in which case it has no effect. see CPython's object.h
    subroutine Py_XDecRef(obj)
        implicit none
        type(C_PTR), value :: obj
        type(C_PTR) :: tmpobj

        tmpobj = obj
        if (c_associated(tmpobj)) call Py_DecRef(tmpobj)
    end subroutine Py_XDecRef


    !> Decrements ref counter and set object to NULL. See CPython's object.h
    subroutine Py_Clear(obj)
        implicit none
        type(C_PTR), value :: obj
        type(C_PTR) :: tmpobj

        tmpobj = obj
        if (c_associated(tmpobj)) then
            obj = C_NULL_PTR
            call Py_DecRef(tmpobj)
        end if
    end subroutine Py_Clear


end module Object

