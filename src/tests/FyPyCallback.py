# -*- coding: utf-8 -*-
"""
Created on Wed Aug 28 10:26:27 2019

@author: bmahajan
"""

def FyPyCallback(l1=0,ll1 = 0, r1=0, b1=False, c1=0+0j):
    print("Inside Callback function called from FyPy module")
    
    l1 = l1*5
    ll1 = ll1*55
    r1 = r1*5.5
    b1 = not b1
    c1 = c1*5
    
    return (l1,ll1, r1,b1,c1)



