'''This Cython code is an automatically generated wrapper
for Fortran code made by 'fmodpy'. The original documentation
for the Fortran source code follows.

! Test Fortran REAL wrapping and usage from Python with fmodpy.
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
    void c_test_standard(double* sing_in, double* sing_out, int* array_in_dim_1, double* array_in, int* array_out_dim_1, double* array_out, int* known_array_out_dim_1, double* known_array_out, int* known_matrix_out_dim_1, int* known_matrix_out_dim_2, double* known_matrix_out, bint* opt_sing_in_present, double* opt_sing_in, bint* opt_sing_out_present, double* opt_sing_out)

@cython.binding(True)
def test_standard(double sing_in, double[:] array_in, double[:] array_out, known_array_out=None, known_matrix_out=None, opt_sing_in=None, opt_sing_out=None):
    '''! Test the basic functionaly of the 'REAL' type and its
! interoperability with Python. This includes, inputs, outputs,
! array inputs with known and unknown size, optional inputs, and
! optional outputs.'''
    
    # Setting up "sing_out"
    cdef double sing_out
    
    # Setting up "array_in"
    cdef int array_in_dim_1 = array_in.shape[0]
    
    # Setting up "array_out"
    cdef int array_out_dim_1 = array_out.shape[0]
    
    # Setting up "known_array_out"
    cdef int known_array_out_dim_1 = 0
    if (known_array_out is None):
        known_array_out = numpy.zeros(shape=(array_out.size), dtype='float64', order='F')
        known_array_out_dim_1 = known_array_out.shape[0]
    elif (not numpy.asarray(known_array_out).flags.f_contiguous):
        raise(Exception("The numpy array given as argument 'known_array_out' was not f_contiguous."))
    else:
        known_array_out_dim_1 = known_array_out.shape[0]
    cdef double[:] known_array_out_local = known_array_out
    
    # Setting up "known_matrix_out"
    cdef int known_matrix_out_dim_1 = 0
    cdef int known_matrix_out_dim_2 = 0
    if (known_matrix_out is None):
        known_matrix_out = numpy.zeros(shape=(3, array_out.size), dtype='float64', order='F')
        known_matrix_out_dim_1 = known_matrix_out.shape[0]
        known_matrix_out_dim_2 = known_matrix_out.shape[1]
    elif (not numpy.asarray(known_matrix_out).flags.f_contiguous):
        raise(Exception("The numpy array given as argument 'known_matrix_out' was not f_contiguous."))
    else:
        known_matrix_out_dim_1 = known_matrix_out.shape[0]
        known_matrix_out_dim_2 = known_matrix_out.shape[1]
    cdef double[:,:] known_matrix_out_local = known_matrix_out
    
    # Setting up "opt_sing_in"
    cdef bint opt_sing_in_present = True
    if (opt_sing_in is None):
        opt_sing_in_present = False
        opt_sing_in = 1
    cdef double opt_sing_in_local = opt_sing_in
    
    # Setting up "opt_sing_out"
    cdef bint opt_sing_out_present = True
    if (opt_sing_out is None):
        opt_sing_out_present = False
        opt_sing_out = 1
    cdef double opt_sing_out_local = opt_sing_out

    # Call C-accessible Fortran wrapper.
    c_test_standard(&sing_in, &sing_out, &array_in_dim_1, &array_in[0], &array_out_dim_1, &array_out[0], &known_array_out_dim_1, &known_array_out_local[0], &known_matrix_out_dim_1, &known_matrix_out_dim_2, &known_matrix_out_local[0,0], &opt_sing_in_present, &opt_sing_in_local, &opt_sing_out_present, &opt_sing_out_local)

    # Return final results, 'INTENT(OUT)' arguments only.
    return sing_out, numpy.asarray(array_out), numpy.asarray(known_array_out_local), numpy.asarray(known_matrix_out_local), (opt_sing_out_local if opt_sing_out_present else None)


# ----------------------------------------------
# Wrapper for the Fortran subroutine TEST_EXTENDED

cdef extern:
    void c_test_extended(bint* opt_array_in_present, int* opt_array_in_dim_1, double* opt_array_in, bint* known_opt_array_out_present, int* known_opt_array_out_dim_1, double* known_opt_array_out, bint* opt_alloc_array_out_present, int* opt_alloc_array_out_dim_1, double** opt_alloc_array_out, int* alloc_array_out_dim_1, double** alloc_array_out, int* n)

@cython.binding(True)
def test_extended(int n, opt_array_in=None, known_opt_array_out=None, opt_alloc_array_out=None):
    '''! Test the extended functionaly of the 'REAL' type and its
! interoperability with Python. This includes, optional array
! inputs, optional array outputs, and allocatable array outputs.'''
    
    # Setting up "opt_array_in"
    cdef bint opt_array_in_present = True
    cdef int opt_array_in_dim_1 = 0
    if (opt_array_in is None):
        opt_array_in_present = False
        opt_array_in = numpy.zeros(shape=(1), dtype='float64', order='F')
    elif (type(opt_array_in) == bool) and (opt_array_in):
        opt_array_in = numpy.zeros(shape=(1), dtype='float64', order='F')
        opt_array_in_dim_1 = opt_array_in.shape[0]
    elif (not numpy.asarray(opt_array_in).flags.f_contiguous):
        raise(Exception("The numpy array given as argument 'opt_array_in' was not f_contiguous."))
    else:
        opt_array_in_dim_1 = opt_array_in.shape[0]
    cdef double[:] opt_array_in_local = opt_array_in
    
    # Setting up "known_opt_array_out"
    cdef bint known_opt_array_out_present = True
    cdef int known_opt_array_out_dim_1 = 0
    if (known_opt_array_out is None):
        known_opt_array_out_present = False
        known_opt_array_out = numpy.zeros(shape=(1), dtype='float64', order='F')
    elif (type(known_opt_array_out) == bool) and (known_opt_array_out):
        known_opt_array_out = numpy.zeros(shape=(3), dtype='float64', order='F')
        known_opt_array_out_dim_1 = known_opt_array_out.shape[0]
    elif (not numpy.asarray(known_opt_array_out).flags.f_contiguous):
        raise(Exception("The numpy array given as argument 'known_opt_array_out' was not f_contiguous."))
    else:
        known_opt_array_out_dim_1 = known_opt_array_out.shape[0]
    cdef double[:] known_opt_array_out_local = known_opt_array_out
    
    # Setting up "opt_alloc_array_out"
    cdef bint opt_alloc_array_out_present = True
    cdef int opt_alloc_array_out_dim_1 = 0
    if (opt_alloc_array_out is None):
        opt_alloc_array_out_present = False
    cdef double* opt_alloc_array_out_local
    
    # Setting up "alloc_array_out"
    cdef int alloc_array_out_dim_1
    cdef double* alloc_array_out_local

    # Call C-accessible Fortran wrapper.
    c_test_extended(&opt_array_in_present, &opt_array_in_dim_1, &opt_array_in_local[0], &known_opt_array_out_present, &known_opt_array_out_dim_1, &known_opt_array_out_local[0], &opt_alloc_array_out_present, &opt_alloc_array_out_dim_1, &opt_alloc_array_out_local, &alloc_array_out_dim_1, &alloc_array_out_local, &n)

    # Post-processing "opt_alloc_array_out"
    cdef numpy.npy_intp opt_alloc_array_out_size = (opt_alloc_array_out_dim_1)
    if (opt_alloc_array_out_present):
        opt_alloc_array_out = ptr_to_numpy_array(opt_alloc_array_out_size, numpy.NPY_FLOAT64, &opt_alloc_array_out_local[0])
        opt_alloc_array_out = opt_alloc_array_out.reshape((opt_alloc_array_out_dim_1)).T
    else: opt_alloc_array_out = None
    
    # Post-processing "alloc_array_out"
    cdef numpy.npy_intp alloc_array_out_size = (alloc_array_out_dim_1)
    alloc_array_out = ptr_to_numpy_array(alloc_array_out_size, numpy.NPY_FLOAT64, alloc_array_out_local)
    alloc_array_out = alloc_array_out.reshape((alloc_array_out_dim_1)).T
    
    # Return final results, 'INTENT(OUT)' arguments only.
    return (numpy.asarray(known_opt_array_out_local) if known_opt_array_out_present else None), (numpy.asarray(opt_alloc_array_out) if opt_alloc_array_out_present else None), numpy.asarray(alloc_array_out)

