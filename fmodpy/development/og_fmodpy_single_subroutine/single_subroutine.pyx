'''! This routine says hello.'''


import cython
import numpy

class NotFortranCompatible(Exception): pass

#      Wrapper for fortran function say_hello     
# =================================================

cdef extern:
    void c_say_hello( long* a, double* b, int* c, float* d )

@cython.boundscheck(False)
@cython.wraparound(False)
def say_hello( long a, double b, c=None, d=None ):
    ''''''
    # Prepare for fortran function call (initialize optionals)
    if (type(c) == type(None)):
        c = 1
    cdef int local_c = c
    
    if (type(d) == type(None)):
        d = 1
    cdef float local_d = d
    
    
    # Make fortran function call
    c_say_hello(&a, &b, &local_c, &local_d)
    # Return appropriate values based on "INTENT" from fortran code
    
    return local_c, local_d



