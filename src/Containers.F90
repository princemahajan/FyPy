!############################################################################################
!
!  FyPy
!
!> \brief       Container Data Structure Module
!! \details     It provides Fortran API for Container objects of Python/C API using Sequence
!!              protocol in addition to String, Tuple, List, and Dictionary object protocol.
!! \author      Bharat Mahajan
!! \date        08/13/2019    
!
!############################################################################################


module Containers

    use, intrinsic :: iso_c_binding, only: C_CHAR, C_PTR, C_INT
    use Object, only: PY_SSIZE_T

    implicit none

    public

    interface

    !###################
    ! String objects API
    !###################

    !> Convert Fortran string to PyObject string
    type(C_PTR) function PyUnicode_DecodeUTF8(str, size, errstr) &
                            bind(C,name='PyUnicode_DecodeUTF8')
        import :: PY_SSIZE_T, C_PTR, C_CHAR
        implicit none
        character(len=1), dimension(*), intent(in) :: str
        integer(PY_SSIZE_T), value :: size
        character(len=1), dimension(*), intent(in) :: errstr
    end function PyUnicode_DecodeUTF8


    !> convert Python string object to C string and also return size in bytes
    type(C_PTR) function PyUnicode_AsUTF8AndSize(obj, size) &
                            bind(C, name = 'PyUnicode_AsUTF8AndSize')
        import :: PY_SSIZE_T, C_PTR
        implicit none
        type(C_PTR), value :: obj
        integer(PY_SSIZE_T) :: size
    end function PyUnicode_AsUTF8AndSize

    
    !##################
    ! Sequence Protocol
    !##################
    
    !> Returns 1 if obj provides sequence protocol and 0 otherwise. It returns
    !! 1 for Python classes with a __getitem()__ method.
    integer(C_INT) function PySequence_Check(seq) bind(C, name='PySequence_Check')
        import :: C_INT, C_PTR
        implicit none
        type(C_PTR), value :: seq
    end function PySequence_Check


    integer(PY_SSIZE_T) function PySequence_Size(seq) bind(C, name='PySequence_Size')
        import :: PY_SSIZE_T, C_PTR
        implicit none
        type(C_PTR), value :: seq
    end function PySequence_Size

    integer(PY_SSIZE_T) function PySequence_Length(seq) bind(C, name='PySequence_Length')
        import :: PY_SSIZE_T, C_PTR
        implicit none
        type(C_PTR), value :: seq
    end function PySequence_Length

    !> Returns the concatenation of obj1 and obj2 on success and NULL on failure. New reference
    type(C_PTR) function PySequence_Concat(seq1, seq2) bind(C, name='PySequence_Concat')
        import :: C_INT, C_PTR
        implicit none
        type(C_PTR), value :: seq1, seq2
    end function PySequence_Concat

    !> Returns the result of repeat of seq count times on success and NULL on failure. New reference
    type(C_PTR) function PySequence_Repeat(seq, count) bind(C, name='PySequence_Repeat')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: seq
        integer(PY_SSIZE_T), value :: count
    end function PySequence_Repeat

    !> Returns the element at index or NULL on failure. New reference
    type(C_PTR) function PySequence_GetItem(seq, index) bind(C, name='PySequence_GetItem')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: seq
        integer(PY_SSIZE_T), value :: index
    end function PySequence_GetItem

    !> Returns the slice of sequence between i1 and i2 or NULL on failure. New reference
    type(C_PTR) function PySequence_GetSlice(seq, i1,i2) bind(C, name='PySequence_GetSlice')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: seq
        integer(PY_SSIZE_T), value :: i1,i2
    end function PySequence_GetSlice

    !> Assign item to the index element of seq. Raise exception and return -1 on failure.
    !! Returns 0 on success. It does not steal a reference.
    integer(C_INT) function PySequence_SetItem(seq, index, item) bind(C, name='PySequence_SetItem')
        import :: C_INT, C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: seq
        integer(PY_SSIZE_T) :: index
        type(C_PTR), value :: item
    end function PySequence_SetItem
    
    !> Delete item at the index  of seq. Returns -1 on failure.
    integer(C_INT) function PySequence_DelItem(seq, index) bind(C, name='PySequence_DelItem')
        import :: C_INT, C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: seq
        integer(PY_SSIZE_T) :: index
    end function PySequence_DelItem
    

    !> Assign object v to seq from i1 to i2. 
    integer(C_INT) function PySequence_SetSlice(seq, i1, i2, v) bind(C, name='PySequence_SetSlice')
        import :: C_INT, C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: seq
        integer(PY_SSIZE_T) :: i1,i2
        type(C_PTR), value :: v
    end function PySequence_SetSlice
    
    !> Delete slice in seq from i1 to i2. Returns -1 on failure. 
    integer(C_INT) function PySequence_DelSlice(seq, i1, i2) bind(C, name='PySequence_DelSlice')
        import :: C_INT, C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: seq
        integer(PY_SSIZE_T) :: i1,i2
    end function PySequence_DelSlice

    !> Return the number of occurrences of val in seq. Returns -1 on failure. 
    integer(PY_SSIZE_T) function PySequence_Count(seq, val) bind(C, name='PySequence_Count')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: seq, val
    end function PySequence_Count

    !> Determines if seq contains val. If it is return 1 else 0. On error, returns -1.
    integer(C_INT) function PySequence_Contains(seq, val) bind(C, name='PySequence_Contains')
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: seq, val
    end function PySequence_Contains

    !> Return the 1st index i for which o[i]==val. Returns -1 on failure. 
    integer(PY_SSIZE_T) function PySequence_Index(seq, val) bind(C, name='PySequence_Index')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: seq, val
    end function PySequence_Index

    !> Returns a list object with the same contents as sequence or iterable obj, or NULL on failure.
    !! Same as list(obj). New reference.
    type(C_PTR) function PySequence_List(obj) bind(C, name='PySequence_List')
        import :: C_PTR
        implicit none
        type(C_PTR), value :: obj
    end function PySequence_List

    !> Returns a tuple object with the same contents as sequence or iterable obj, or NULL on failure.
    !! Same as tuple(obj). New reference.
    type(C_PTR) function PySequence_Tuple(obj) bind(C, name='PySequence_Tuple')
        import :: C_PTR
        implicit none
        type(C_PTR), value :: obj
    end function PySequence_Tuple


    !##################
    ! Tuple objects API
    !##################

    !> Creates a new tuple object of given length, see tupleobject.h (new reference)
    type(C_PTR) function PyTuple_New(len) bind(C, name='PyTuple_New')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        integer(PY_SSIZE_T), value :: len
    end function PyTuple_New

    !> Returns size of the tuple, see tupleobject.h
    integer(PY_SSIZE_T) function PyTuple_Size(tuple) bind(C, name='PyTuple_Size')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: tuple
    end function PyTuple_Size


    !> Returns the object in the tuple at given position, see tupleobject.h (borrowed reference)
    type(C_PTR) function PyTuple_GetItem(tuple, pos) bind(C, name='PyTuple_GetItem')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: tuple
        integer(PY_SSIZE_T), value :: pos
    end function PyTuple_GetItem

    !> Returns the slice of the tuple from low to high (new reference)
    type(C_PTR) function PyTuple_GetSlice(tuple, low, high) bind(C, name='PyTuple_GetSlice')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: tuple
        integer(PY_SSIZE_T), value :: low, high
    end function PyTuple_GetSlice

    !> Insert a reference to an object at position pos of the tuple, returns 0 on success
    !! It steals the reference to obj
    integer(C_INT) function PyTuple_SetItem(tuple, pos, obj) bind(C, name='PyTuple_SetItem')
        import :: C_PTR, PY_SSIZE_T, C_INT
        implicit none
        type(C_PTR), value :: tuple, obj
        integer(PY_SSIZE_T), value :: pos
    end function PyTuple_SetItem

    !> clears the tuple free list
    integer(C_INT) function PyTuple_ClearFreeList() bind(C, name='PyTuple_ClearFreeList')
        import :: C_INT
        implicit none
    end function PyTuple_ClearFreeList



    !########################
    ! Python List objects API
    !########################

    !> Creates a new list object, New reference. Returns NULL on failure.
    !! If len is greater than 0, then returned list items are set to NULL.
    type(C_PTR) function PyList_New(len) bind(C, name='PyList_New')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        integer(PY_SSIZE_T), value :: len
    end function PyList_New


    !> Returns the length of the list
    integer(PY_SSIZE_T) function PyList_Size(list) bind(C, name='PyList_Size')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: list
    end function PyList_Size


    !> Returns the object in the list at given position (borrowed reference)
    !! Sets IndexError exception if index out of bounds
    type(C_PTR) function PyList_GetItem(list, index) bind(C, name='PyList_GetItem')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: list
        integer(PY_SSIZE_T), value :: index !< must be non-negative
    end function PyList_GetItem

    !> Returns a list of the objects from the list between low and high (new reference)
    !! Returns NULL in case of an exception. No negative indices.
    type(C_PTR) function PyList_GetSlice(list, low, high) bind(C, name='PyList_GetSlice')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: list
        integer(PY_SSIZE_T), value :: low, high
    end function PyList_GetSlice

    !> Set the item at index in list to item. Returns 0 on success, otherwise -1.
    !! It steals the reference to item
    integer(C_INT) function PyList_SetItem(list, pos, item) bind(C, name='PyList_SetItem')
        import :: C_PTR, PY_SSIZE_T, C_INT
        implicit none
        type(C_PTR), value :: list, item
        integer(PY_SSIZE_T), value :: pos
    end function PyList_SetItem

    !> Set the slice of list between low and high to contents of itemlist. No negative indices.
    !! Basically list[low:high] = itemlist. Returns 0 on success, -1 on failure. 
    integer(C_INT) function PyList_SetSlice(list, low, high, itemlist) bind(C, name='PyList_SetSlice')
        import :: C_PTR, PY_SSIZE_T, C_INT
        implicit none
        type(C_PTR), value :: list, itemlist
        integer(PY_SSIZE_T), value :: low,high
    end function PyList_SetSlice


    !> Inserts item into the list in front of item at index. Returns 0 on success, otherwise -1.
    integer(C_INT) function PyList_Insert(list, index, item) bind(C, name='PyList_Insert')
        import :: C_PTR, PY_SSIZE_T, C_INT
        implicit none
        type(C_PTR), value :: list, item
        integer(PY_SSIZE_T), value :: index
    end function PyList_Insert

    !> Appends item into the list at the end. Returns 0 on success, otherwise -1.
    integer(C_INT) function PyList_Append(list, item) bind(C, name='PyList_Append')
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: list, item
    end function PyList_Append

    !> Sorts the list. Returns 0 on success, otherwise -1.
    integer(C_INT) function PyList_Sort(list) bind(C, name='PyList_Sort')
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: list
    end function PyList_Sort

    !> Reverse the items of the list. Returns 0 on success, otherwise -1.
    integer(C_INT) function PyList_Reverse(list) bind(C, name='PyList_Reverse')
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: list
    end function PyList_Reverse

    !> Returns a new tuple containing list contents. New reference.
    type(C_PTR) function PyList_AsTuple(list) bind(C, name='PyList_AsTuple')
        import :: C_PTR
        implicit none
        type(C_PTR), value :: list
    end function PyList_AsTuple

    !> Returns a new tuple containing list contents. New reference.
    integer(C_INT) function PyList_ClearFreeList() bind(C, name='PyList_ClearFreeList')
        import :: C_INT
        implicit none
    end function PyList_ClearFreeList


    !#######################
    ! Dictionary objects API
    !#######################

    !> Creates a new dictionary object, see dictobject.h (new reference)
    type(C_PTR) function PyDict_New() bind(C, name='PyDict_New')
        import :: C_PTR, PY_SSIZE_T
        implicit none
    end function PyDict_New

    !> Empty an existing dictionary object
    subroutine PyDict_Clear(dict) bind(C, name='PyDict_Clear')
        import :: C_PTR
        implicit none
        type(C_PTR), value :: dict
    end subroutine PyDict_Clear

    !> Query a key in dictionary, on match returns 1, otherwise 0. On error, returns -1.
    integer(PY_SSIZE_T) function PyDict_Contains(dict, key) bind(C, name='PyDict_Contains')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: dict, key
    end function PyDict_Contains

    !> Copy a dictionary, returns new reference
    type(C_PTR) function PyDict_Copy(dict) bind(C, name='PyDict_Copy')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: dict
    end function PyDict_Copy

    !> Insert value into the dictionary dict using key as key that is hashable.
    !! Return 0 on success, else -1. 
    integer(C_INT) function PyDict_SetItem(dict, key, val) bind(C, name='PyDict_SetItem')
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: dict, key, val
    end function PyDict_SetItem

    !> Insert value into the dictionary with string key  as a key.
    !! Return 0 on success, else -1. 
    integer(C_INT) function PyDict_SetItemString(dict, key, val) bind(C, name='PyDict_SetItemString')
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: dict, val
        character(len=1), dimension(*), intent(in) :: key
    end function PyDict_SetItemString

    !> Removes the value from the dictionary dict with key as key that is hashable.
    !! Return 0 on success, else -1. 
    integer(C_INT) function PyDict_DelItem(dict, key) bind(C, name='PyDict_DelItem')
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: dict, key
    end function PyDict_DelItem

    !> Removes the value from the dictionary with string key as a key.
    !! Return 0 on success, else -1. 
    integer(C_INT) function PyDict_DelItemString(dict, key) bind(C, name='PyDict_DelItemString')
        import :: C_PTR, C_INT
        implicit none
        type(C_PTR), value :: dict
        character(len=1), dimension(*), intent(in) :: key
    end function PyDict_DelItemString

    !> Return the object from the dictionary dict with key as key that is hashable.
    !! Returns borrowed reference. Returns NULL if key is not present with no exception setting. 
    type(C_PTR) function PyDict_GetItem(dict, key) bind(C, name='PyDict_GetItem')
        import :: C_PTR
        implicit none
        type(C_PTR), value :: dict, key
    end function PyDict_GetItem

    !> Return the object from the dictionary dict with string key as the key.
    !! Returns borrowed reference. Returns NULL if key is not present with no exception setting. 
    type(C_PTR) function PyDict_GetItemString(dict, key) bind(C, name='PyDict_GetItemString')
        import :: C_PTR
        implicit none
        type(C_PTR), value :: dict
        character(len=1), dimension(*), intent(in) :: key
    end function PyDict_GetItemString

    !> Returns a PyListObject containung all the items from dict, new reference
    type(C_PTR) function PyDict_Items(dict) bind(C, name='PyDict_Items')
        import :: C_PTR
        implicit none
        type(C_PTR), value :: dict
    end function PyDict_Items

    !> Returns a PyListObject containung all the keys from dict, new reference
    type(C_PTR) function PyDict_Keys(dict) bind(C, name='PyDict_Keys')
        import :: C_PTR
        implicit none
        type(C_PTR), value :: dict
    end function PyDict_Keys

    !> Returns a PyListObject containung all the keys from dict, new reference
    type(C_PTR) function PyDict_Values(dict) bind(C, name='PyDict_Values')
        import :: C_PTR
        implicit none
        type(C_PTR), value :: dict
    end function PyDict_Values

    !> Returns the size of the dictionary
    integer(PY_SSIZE_T) function PyDict_Size(dict) bind(C, name='PyDict_Size')
        import :: C_PTR, PY_SSIZE_T
        implicit none
        type(C_PTR), value :: dict
    end function PyDict_Size


    end interface



    contains





end module Containers

