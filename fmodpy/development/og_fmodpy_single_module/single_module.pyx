'''! This file contains a single test module. It also demonstrates basic
! Fortran syntax and conveys some expectations made by `fmodpy` when
! parsing code.
! Documentation for the module "TEST" can go in a comment immediately
! before the module itself.
! Documentation can also go immediately inside the module, this will
! be included in addition to preceding comments if both exist.
! Documentation for the function "ADD" can go immediately before the
! subroutine. This subroutine takes three arguments that would be
! considered "pass by reference" in C. Only arguments with INTENT(OUT)
! are allowed to be modified by the body code (enforced by compilers).'''


import cython
import numpy

class NotFortranCompatible(Exception): pass

#      Wrapper for fortran function add     
# =================================================

cdef extern:
    void c_add( double* add_a, double* add_b, double* add_c )

@cython.boundscheck(False)
@cython.wraparound(False)
def add( double add_a, double add_b, add_c=None ):
    '''! Sometimes documentation goes on the inside too! When it does,
    ! both the outer and inner documentation will be preserved.'''
    # Prepare for fortran function call (initialize optionals)
    if (type(add_c) == type(None)):
        add_c = 1
    cdef double local_add_c = add_c
    
    
    # Make fortran function call
    c_add(&add_a, &add_b, &local_add_c)
    # Return appropriate values based on "INTENT" from fortran code
    
    return local_add_c



#      Wrapper for fortran function tautology     
# =================================================

cdef extern:
    void c_tautology( bint* yes_output )

@cython.boundscheck(False)
@cython.wraparound(False)
def tautology( yes_output=None ):
    ''''''
    # Prepare for fortran function call (initialize optionals)
    if (type(yes_output) == type(None)):
        yes_output = 1
    cdef bint local_yes_output = yes_output
    
    
    # Make fortran function call
    c_tautology(&local_yes_output)
    # Return appropriate values based on "INTENT" from fortran code
    
    return local_yes_output



#      Wrapper for fortran function falsehood     
# =================================================

cdef extern:
    void c_falsehood( bint* falsehood_output )

@cython.boundscheck(False)
@cython.wraparound(False)
def falsehood( falsehood_output=None ):
    ''''''
    # Prepare for fortran function call (initialize optionals)
    if (type(falsehood_output) == type(None)):
        falsehood_output = 1
    cdef bint local_falsehood_output = falsehood_output
    
    
    # Make fortran function call
    c_falsehood(&local_falsehood_output)
    # Return appropriate values based on "INTENT" from fortran code
    
    return local_falsehood_output



#      Wrapper for fortran function subtract     
# =================================================

cdef extern:
    void c_subtract( float* sub_a, float* sub_b, float* sub_c_output )

@cython.boundscheck(False)
@cython.wraparound(False)
def subtract( float sub_a, float sub_b, sub_c_output=None ):
    ''''''
    # Prepare for fortran function call (initialize optionals)
    if (type(sub_c_output) == type(None)):
        sub_c_output = 1
    cdef float local_sub_c_output = sub_c_output
    
    
    # Make fortran function call
    c_subtract(&sub_a, &sub_b, &local_sub_c_output)
    # Return appropriate values based on "INTENT" from fortran code
    
    return local_sub_c_output



#      Wrapper for fortran function multiply     
# =================================================

cdef extern:
    void c_multiply( float* mult_a, long* mult_b, float* multiply_output )

@cython.boundscheck(False)
@cython.wraparound(False)
def multiply( float mult_a, long mult_b, multiply_output=None ):
    '''! Documentation for the function "MULTIPLY". Lots of (mathematical)
    ! software strictly has documentation inside the FUNCTION / SUBROUTINE.'''
    # Prepare for fortran function call (initialize optionals)
    if (type(multiply_output) == type(None)):
        multiply_output = 1
    cdef float local_multiply_output = multiply_output
    
    
    # Make fortran function call
    c_multiply(&mult_a, &mult_b, &local_multiply_output)
    # Return appropriate values based on "INTENT" from fortran code
    
    return local_multiply_output



