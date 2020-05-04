'''This Cython code is an automatically generated wrapper
for Fortran code made by 'fmodpy'. The original documentation
for the Fortran source code follows.

! This file contains a single test module. It also demonstrates basic
! Fortran syntax and conveys some expectations made by `fmodpy` when
! parsing code.'''

import cython
import numpy
class NotFortranCompatible(Exception): pass


# ----------------------------------------------
# Wrapper for the Fortran subroutine ADD

cdef extern:
    void c_add(double* add_a, double* add_b, double* add_c)

def add(double add_a, double add_b, add_c=None):
    '''! Documentation for the function "ADD" can go immediately before the
! subroutine. This subroutine takes three arguments that would be
! considered "pass by reference" in C. Only arguments with INTENT(OUT)
! are allowed to be modified by the body code (enforced by compilers).
! Sometimes documentation goes on the inside too! When it does,
! both the outer and inner documentation will be preserved.'''
    # Argument declaration code.
    if (add_c is None):
        add_c = 1
    cdef double add_c_local = add_c

    # Fortran C wrapper function call.
    c_add(&add_a, &add_b, &add_c_local)

    # Return final results, 'INTENT(OUT)' arguments only.
    return add_c_local


# ----------------------------------------------
# Wrapper for the Fortran subroutine TAUTOLOGY

cdef extern:
    void c_tautology(bint* yes)

def tautology(yes=None):
    ''''''
    # Argument declaration code.
    if (yes is None):
        yes = 1
    cdef bint yes_local = yes

    # Fortran C wrapper function call.
    c_tautology(&yes_local)

    # Return final results, 'INTENT(OUT)' arguments only.
    return yes_local


# ----------------------------------------------
# Wrapper for the Fortran subroutine FALSEHOOD

cdef extern:
    void c_falsehood(bint* falsehood)

def falsehood(falsehood=None):
    ''''''
    # Argument declaration code.
    if (falsehood is None):
        falsehood = 1
    cdef bint falsehood_local = falsehood

    # Fortran C wrapper function call.
    c_falsehood(&falsehood_local)

    # Return final results, 'INTENT(OUT)' arguments only.
    return falsehood_local


# ----------------------------------------------
# Wrapper for the Fortran subroutine SUBTRACT

cdef extern:
    void c_subtract(float* sub_a, float* sub_b, float* sub_c)

def subtract(float sub_a, float sub_b, sub_c=None):
    '''! Documentation for the function "SUBTRACT". This routine is using
! the default "REAL" precision (usually 32 bits).'''
    # Argument declaration code.
    if (sub_c is None):
        sub_c = 1
    cdef float sub_c_local = sub_c

    # Fortran C wrapper function call.
    c_subtract(&sub_a, &sub_b, &sub_c_local)

    # Return final results, 'INTENT(OUT)' arguments only.
    return sub_c_local


# ----------------------------------------------
# Wrapper for the Fortran subroutine MULTIPLY

cdef extern:
    void c_multiply(float* mult_a, long* mult_b, float* multiply)

def multiply(float mult_a, long mult_b, multiply=None):
    '''! Documentation for the function "MULTIPLY". Lots of (mathematical)
! software strictly has documentation inside the FUNCTION / SUBROUTINE.'''
    # Argument declaration code.
    if (multiply is None):
        multiply = 1
    cdef float multiply_local = multiply

    # Fortran C wrapper function call.
    c_multiply(&mult_a, &mult_b, &multiply_local)

    # Return final results, 'INTENT(OUT)' arguments only.
    return multiply_local


