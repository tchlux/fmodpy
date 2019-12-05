<p align="center">
  <h1 align="center"><code>fmodpy</code></h1>
</p>

<p align="center">
An easy-to-use Fortran wrapper for Python.
</p>

Modern Fortran is capable of being integrated with Python near
seamlessly allowing for rapid transition between prototype and
optimized production-ready code. This packages aims to make Fortran
code as easy to import and use as native Python modules. This combines
the performance of Fortran with the convenience and accessibility of
Python, allowing for a productive and exciting development pipeline.


## INSTALLATION:

    $ pip install fmodpy

  This code expects that you already have a Fortran compiler and a C
  compiler installed. By default, many C compilers do not have access
  to the Fortran header files (for Fortran intrinsics like memory
  allocation and math operations) so if you experience linking errors,
  then use something like `locate libgfortran.a` (for whatever your
  Fortran compiler header file will be named) and set the C
  `ld_shared_path` argument to that directory path.

  Perhaps the easiest setup is to install `gfortran` and `gcc` with
  your preferred package manager, then default behavior should work.


## USAGE:

### PYTHON:

```python
import fmodpy

# Compile and import the Fortran code. (This will automatically
#  recompile the code if it has been saved recently.)
module = fmodpy.fimport("<fortran source file>",
                        f_compiler="gfortran",
                        c_linker="gcc", c_link_args=["-lgfortran"])
```

  For more details, see the `help(fmodpy.fimport)` documentation.

### COMMAND LINE:

  For details on the different options, run python interactively and 
  look at `help(fmodpy.wrap)`. The execution from the command line looks like:

    $ python -m fmodpy <fortran source file> [<fmodpy.wrap kwargs>] [<fmodpy.globals() kwargs>] [<functions to wrap>]

  This outputs a <fortran mod name>.so python module that can be
  imported as any other python module would be.


## HOW IT WORKS:

  Reads the fortran file, abstracting out the modules, subroutines,
  functions. Identifies the type-description of each argument for
  subroutines and functions. Uses type-descriptors to generate a
  minimal cython wrapper and fortran wrapper. Afterwards a module is
  generated using python distutils' setup.py. When importing and
  using the generated fortran module, the call sequence looks like:

       Python code
    -> Cython
    -> Wrapped fortran (transferring characters, implicit shapes, etc.)
    -> Original fortran


  This uses the specifications from the fortran file to determine how
  the interface for each subroutine / function should behave. (I.e.,
  `INTENT(IN)` does not return, `INTENT(OUT)` is optional as input
  when size can be inferred.)


## VERSION HISTORY:

  See [this file](fmodpy/about/version_history.txt).


## UPCOMING:

  This module is undergoing a major code re-base. The interface should
  remain (almost) identical, however some redundant or poorly named
  arguments may be removed or renamed. The changes will allow for the
  following features that are **not** currently supported:

 - multiple Fortran modules in one file
 - custom `TYPE` in Fortran (and arrays of that type)
 - returned `ALLOCATABLE` arrays in Fortran
 - automatically generated `Makefile` and `make.py` scripts for
   distribution that do *not* require other users to have `fmodpy`
 - project-specific `fmodpy` configuration files
 - a default global `fmodpy` configuration file

  Along with these improvements, the code is being modularized to
  allow for easier sustained development. I've been working on this
  throughout 2019, and hope to have the work completed sometime in
  2020.


## EXAMPLE CODE

Here is a simple python code that compares a `fmodpy`-wrapped Fortran
code with standard NumPy. This example performs a matrix multiply
operation using Fortran.

```python
# This is a Python file named whatever you like. It demonstrates
#   automatic `fmodpy` wrapping.

import fmodpy
import numpy as np

code = fmodpy.fimport("code.f03",
                      f_compiler="gfortran",
                      c_linker="gcc",
                      c_link_args=[] # "-lgfortran" might be needed.
)

a = np.array([
    [1,2,3,4,5],
    [1,2,3,4,5]
], dtype=float, order='F')

b = np.array([
    [1,2],
    [3,4],
    [5,6],
    [7,8],
    [9,10]
], dtype=float, order='F')

print()
print("a:")
print(a)

print()
print("b:")
print(b)

print()
print("Numpy result")
print(np.matmul(a,b))

print()
print("Fortran result")
print(code.matrix_multiply(a,b))


print()
help(code.matrix_multiply)
```

Here is the associated Fortran file `code.f03` (no specific reason for
the name, any of `.f95`, `.f03`, and `.f08` extensions could be used).

```fortran
! This module provides various testbed routines for demonstrating
! the simplicity of Fortran code usage with `fmodpy`.
! 
! Contains:
! 
!   MATRIX_MULTIPLY  --  A routine for multiplying two matrices of floats.
! 

SUBROUTINE MATRIX_MULTIPLY(A,B,OUT)
  ! This subroutine multiplies the matrices A and B.
  ! 
  ! INPUT:
  !   A(M,N)  --  A 2D matrix of 64 bit floats.
  !   B(N,P)  --  A 2D matrix of 64 bit floats,
  ! 
  ! OUTPUT:
  !   OUT(M,P)  --  The matrix that is the result of (AB).
  ! 
  USE ISO_FORTRAN_ENV, ONLY: REAL64 ! <- Get a float64 type.
  IMPLICIT NONE  ! <- Make undefined variable usage raise errors.
  REAL(KIND=REAL64), INTENT(IN),  DIMENSION(:,:) :: A, B
  REAL(KIND=REAL64), INTENT(OUT), DIMENSION(SIZE(A,1),SIZE(B,2)) :: OUT

  ! Compute the matrix multiplication of A and B.
  OUT(:,:) = MATMUL(A,B)

END SUBROUTINE MATRIX_MULTIPLY
```

Now run the python program and see the output!
