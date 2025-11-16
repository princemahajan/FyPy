# -*- coding: utf-8 -*-
"""
Created on Thu Aug 29 20:41:53 2019

@author: bmahajan
"""

import sys, os, importlib

#os.chdir(r"C:\Users\bmahajan\data\Repositories\JSC\Poincare\packages\FyPy\build\Debug")
os.chdir(r"C:\Users\bmahajan\data\Repositories\JSC\Poincare\bin")
sys.path.append(os.getcwd())

import TestFyPy

os.chdir(r"C:\Users\bmahajan\data\Repositories\JSC\Poincare\packages\FyPy\src\\tests")
import FyPyCallback as fypy

import numpy as np

print(dir(TestFyPy))

# simple function wit.h arguments
print(TestFyPy.test(1,111,1.1,True,1+1j))


# function with keyword arguments. 
print(TestFyPy.testkw(1,111,1.1,True,1+1j,b1=False, l1 = 99,ll1=999999,y1=9.999))

# callback function with keyword arguments
print(TestFyPy.testcb(fypy.FyPyCallback, 1,111,1.1,True,10+10j))


# create numpy arrays for inputs
i8arr = np.array([3],np.int8,order='F')
i32arr = np.array([[1,2],[3,4]],np.int32,order='F')
i64arr = np.array([[[1,2,3],[4,5,6],[7,8,9]],[[1,2,3],[4,5,6],[7,8,9]],[[1,2,3],[4,5,6],[7,8,9]]],np.int64,order='F')

r32arr = np.ndarray([2,2,2,2],np.float32,order='F')
r32arr[0,0,:,:] = 0.1
r32arr[0,1,:,:] = 0.2
r32arr[1,0,:,:] = 0.3
r32arr[1,1,:,:] = 0.4

r64arr = np.ndarray([3,5],np.float64,order='F')
r64arr[0,:] = 1.1E-4
r64arr[1,:] = 2.2E-4
r64arr[2,:] = 3.3E-4

# simple function with input arguments parsing
print(TestFyPy.testparse(1,999999,1.1,True,1+1j,i8arr,i32arr,i64arr,r32arr,r64arr))
