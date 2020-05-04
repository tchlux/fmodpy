''''''


import cython
import numpy

class NotFortranCompatible(Exception): pass

#      Interface for fortran function quadratic     
# =================================================

cdef void c_quadratic(float* x, float* y):
    # Recall the python function from global 
    #  (this was assigned by cython during python wrapper call)
    global quadratic_global
    # Define any variables needed to copy arrays
    
    # Create local variables that are python types
    
    # Call function with python variables in and out
    y[0] = quadratic_global(x[0])
    # Copy out from python variables back into c-typed (fortran) variables
    

#      Wrapper for fortran function test_standard     
# =================================================

cdef extern:
    void c_test_standard( void (*quadratic)(float*, float*), int* steps, float* solution, float* max_window, bint* max_window_present )

@cython.boundscheck(False)
@cython.wraparound(False)
def test_standard( quadratic, int steps, float solution, max_window=None ):
    '''! Minimize a quadratic by taking random'''
    # Prepare for fortran function call (initialize optionals)
    global quadratic_global
    quadratic_global = quadratic
    cdef bint max_window_present = True
    if (type(max_window) == type(None)):
        max_window_present = False
        max_window = 1
    cdef float local_max_window = max_window
    
    
    # Make fortran function call
    c_test_standard(&c_quadratic, &steps, &solution, &local_max_window, &max_window_present)
    # Return appropriate values based on "INTENT" from fortran code
    
    return solution



