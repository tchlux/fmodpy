|**TITLE:**   | fmodpy                                                                 |
|**PURPOSE:** | A lightweight, efficient, highly-automated, fortran wrapper for python.|
|**AUTHOR:**  | Thomas C.H. Lux                                                        |
|**EMAIL:**   | tchlux@vt.edu                                                          |


## INSTALLATION:

    $ pip install fmodpy

## PYTHON USAGE:

    import fmodpy
    fmodpy.wrap("<fortran source file>")
    import <fortran_as_module>


## COMMAND LINE USAGE:

  For details on the different options, run python interactively and 
  look at help(fmodpy.wrap). The execution of the program looks like:

    $ python fmodpy.py <fortran source file> [<fmodpy.wrap kwargs>] [<fmodpy.globals() kwargs>] [<functions to wrap>]

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
  the interface for each subroutine / function should behave. (i.e.
  INTENT(IN) does not return, INTENT(OUT) is optional as input)


## VERSION HISTORY:

|Version and Date       | Description           |
|-----------------------|-----------------------|
| 0.0.0<br>October 2017 | First stable release, handles integers, reals, <br> characters, logicals, and procedures (as arguments). <br> Compiles on Ubuntu, Mac OS, and Windows using <br> gcc as the linker, gcc / clang / gcc respectively <br> as the compilers. |


## UPCOMING MODIFICATIONS:

TODO: Make sure fortran argument names do not conflict with reserved
      words in python, if they do, then add a reserved-word previx

TODO: Add scanner that automatically parses the first few lines of
      python file that imported fmodpy to do automatic compilcation.

TODO: Make intermediate fortran subroutine that passes assumed
      shapes back into C (for passing subroutines as arguments).

TODO: Add support for fortran data types (structs in C).

TODO: Restructure fortran parsing to be based on a grammar (like
      python itself). Use this grammar to parse individual lines of
      fortran code instead of the list/conditional mechanism I have.
