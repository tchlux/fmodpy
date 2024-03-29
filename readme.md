<p align="center">
  <h1 align="center"><code>fmodpy</code></h1>
</p>

<p align="center">
An easy-to-use Fortran wrapper for Python.
<br>
No source code changes, no manual specification files, just import like it's native.
</p>

```python
# Automatically generate a wrapper, compile, link, and import into Python.
fortran_module = fmodpy.fimport('MODULE.f08', dependencies=['LIB.f90', 'BLAS.f'])
#   - handles any Fortran source, modern, or fixed format F77
#   - reads the source to determine the interface specification
#   - includes Fortran documentation in the wrapped code
#   - produces a stand-alone and distrutable Python module with only numpy as dependency
```


Modern Fortran is capable of being integrated with Python near
seamlessly allowing for rapid transition between prototype and
optimized production-ready code. This packages aims to make Fortran
code as easy to import and use as native Python. This combines the
performance of Fortran with the convenience and accessibility of
Python, allowing for a productive and exciting development pipeline.

This package is compiler independent. The generated wrappers are 
self contained, written purely in Python, and are immediately sharable
to any other POSIX platform with a Fortran compiler installed. The only
python dependency is `numpy` (no need to hassle with installing `cython`).
After generating the python wrapper module, even `fmodpy` itself is no longer 
a dependency! This makes `fmodpy` particularly suitable to writing and sharing
performant mathematical software with a broader audience, since
complex dependencies can often be difficult to configure on cutting
edge high performance computing platforms.


## INSTALLATION:

    python3 -m pip install fmodpy

  This code expects that you already have a Fortran compiler
  installed. By default most machines do not have a Fortran compiler
  installed, but most package managers support installation of
  `gfortran` (a GNU compiler). In addition, there are popular
  commercial Fortran compilers such as `pgifortran` (the [PGI
  compiler](https://www.pgroup.com/products/index.htm) that uniquely
  supports OpenACC), `ifort` (the [Intel
  compiler](https://software.intel.com/content/www/us/en/develop/tools/oneapi/components/fortran-compiler.html)),
  and `f90` (the [Oracle Sun
  compiler](https://www.oracle.com/application-development/technologies/developerstudio-features.html)).

  The easiest setup is to install `gfortran` with your preferred
  package manager, then the default behaviors of `fmodpy` will work
  correctly.


## USAGE:

### PYTHON:

```python
import fmodpy

# Compile and import the Fortran code. (This will automatically
#  recompile the module if the Fortran source has been saved 
#  more recently than the last time the module was imported.)
module = fmodpy.fimport("<path to fortran source file>")
```

  For more details, see the `help(fmodpy.fimport)` documentation.
  Notably, global configurations (i.e., the default Fortran compiler) 
  can be viewed and edited with `fmodpy.configure`.

### COMMAND LINE:

  Run `fmodpy` from the command line with:

  ```shell
  python3 -m fmodpy "<fortran-source-file>" [setting1="<value1>"] [setting2="<value2>"] ...
  ```

  The result will be a directory containing a Python package that
  wraps and calls the underlying Fortran code.

  Execute with no arguments to get help documentation. For a list of
  the different configuration options, run the command:
  ```shell
  python3 -c "import fmodpy; fmodpy.configure()"
  ```


## SUPPORTED FORTRAN:

 - `INTEGER` 32 and 64 bit, (allocatable / assumed shape) arrays, optionals, pointers
 - `REAL` 32 and 64 bit, (allocatable / assumed shape) arrays, optionals, pointers
 - `CHARACTER` singletons and assumed-shape arrays, but *no* support for `LEN` behaviors
 - `COMPLEX` 64 and 128 bit, (allocatable / assumed shape) arrays, optionals, pointers
 - `LOGICAL`  singletons, (allocatable / assumed shape) arrays, optionals, pointers
 - `TYPE` singletons, (allocatable / assumed shape) arrays, optionals, and pointers (type must have `BIND(C)` attribute)
 - `SUBROUTINE` standard behaviors (automatically drops `PURE` and `RECURSIVE` prefixes)
 - `FUNCTION` standard behaviors (wrapped with a standard subroutine call)
 - `MODULE` wrapper that behaves like an instantiated Python class with property-based accessors for internal attributes

It is a goal to eventually allow for the following:
 - passing a `PROCEDURE` as an argument to Fortran code

This code base is entirely driven by concrete examples and use-cases. If you want to see something supported that is not currently, please consider posting a minimum example of the Fortran code you'd like to wrap under the [Issues](https://github.com/tchlux/fmodpy/issues) page.


## HOW IT WORKS:

  Reads the fortran file, abstracting out the modules, subroutines,
  functions. Identifies the type-description of each argument for
  module variables, subroutines, and functions. Uses type-descriptors
  to generate a Fortran wrapper with `BIND(C)` enabled, as well as a
  matching Python wrapper using `ctypes` to pass data from Python into
  the Fortran wrapper. The constructed Python wrapper contains
  compilation settings that will automatically recompile a shared object
  file containing the underlying original Fortran source code. 
  Overall, the call sequence at runtime looks like:

       Python code
    -> Python wrapper converting to C types
    -> Fortran wrapper bound to C (transferring characters, implicit shapes, etc.)
    -> Original Fortran code


  This uses the specifications from the fortran file to determine how
  the interface for each subroutine / function should behave. (I.e.,
  `INTENT(IN)` does not return, `INTENT(OUT)` is optional as input.)


## EXAMPLE CODE

Here is a simple python code that compares a `fmodpy`-wrapped Fortran
code with standard NumPy. This example performs a matrix multiply
operation using Fortran.

```python
# This is a Python file named whatever you like. It demonstrates
#   automatic `fmodpy` wrapping.

import fmodpy
import numpy as np

code = fmodpy.fimport("code.f03")

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
