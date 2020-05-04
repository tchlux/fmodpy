'''This Cython code is an automatically generated wrapper
for Fortran code made by 'fmodpy'. The original documentation
for the Fortran source code follows.


'''

import cython
import numpy
cimport numpy
numpy.import_array()

# C-function for creating a Numpy array over existing allocated memory.
cdef ptr_to_numpy_array(numpy.npy_intp N, int t, void * ptr):
    cdef numpy.ndarray arr = numpy.PyArray_SimpleNewFromData(1, &N, t, ptr)
    return arr
# --------------------------------------------------------------------


# ----------------------------------------------
# Wrapper for the Fortran subroutine TEST_STANDARD

cdef extern:
    void c_test_standard(int* steps, float* solution, bint* max_window_present, float* max_window)

@cython.binding(True)
def test_standard(int steps, float solution, max_window=None):
    ''''''
    
    # Setting up "max_window"
    cdef bint max_window_present = True
    if (max_window is None):
        max_window_present = False
        max_window = 1
    cdef float max_window_local = max_window

    # Call C-accessible Fortran wrapper.
    c_test_standard(&steps, &solution, &max_window_present, &max_window_local)

    # Return final results, 'INTENT(OUT)' arguments only.
    return solution

