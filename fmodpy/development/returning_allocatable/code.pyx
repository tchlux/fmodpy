''''''


import cython
import numpy
from cython cimport view
from libc.stdlib cimport malloc, free

class NotFortranCompatible(Exception): pass

#      Wrapper for fortran function reserve     
# =================================================

cdef extern:
    void c_reserve( long* size, int* output_0, double* output )

@cython.boundscheck(False)
@cython.wraparound(False)
def reserve( long size ):
    ''''''
    # Prepare for fortran function call (initialize optionals)
    cdef int output_0
    cdef double temp
    cdef double* output_ptr = <double *> malloc(1 * sizeof(double))
    
    print("Calling 'reserve'..")

    # Make fortran function call
    c_reserve(&size, &output_0, output_ptr)

    print("Finished 'reserve'..")
    # Return appropriate values based on "INTENT" from fortran code

    # Convert the allocated Fortran array into a cython view
    cdef view.array output
    output = view.array(shape=(size,), itemsize=sizeof(double),
        mode="fortran", allocate_buffer=False)
    output.data = <char *> output_ptr
    # output.callback_free_data = free
    
    return numpy.asarray(output, order='F')



