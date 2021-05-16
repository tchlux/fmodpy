# "util" should perform compilation of fortran codes in setup, saving requires admin privaleges.
# Passed functions store python functions associated with random integer
#   identifier, that is used to look up correct function in cython.

# An example of how to create containers of structures in Numpy
#   https://stackoverflow.com/questions/50822183/efficient-conversion-from-c-array-of-struct-to-numpy-array-with-ctypes
#   https://stackoverflow.com/questions/40063080/casting-an-array-of-c-structs-to-a-numpy-array

from ctypes import Structure, c_char_p, c_int, c_double, POINTER
import numpy as np

def c_func(): pass

class info(Structure):
    _fields_ = [ ("name", c_char_p),
                 ("arr_len", c_int),
                 ("real_data", POINTER(c_double)) ]

c_func.restype = info
ret_val = c_func()
# data = np.ctypeslib.as_array(ret_val.contents.real_data
