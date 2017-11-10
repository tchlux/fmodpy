|             |                                                                        |
|-------------|------------------------------------------------------------------------|
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
  `INTENT(IN)` does not return, `INTENT(OUT)` is optional as input)


## VERSION HISTORY:

|Version and Date       | Description           |
|-----------------------|-----------------------|
| 0.0.0<br>October 2017 | First release, handles integers, reals, <br> characters, logicals, and procedures (as arguments). <br> Compiles on Ubuntu, Mac OS, and Windows using <br> gcc as the linker, gcc / clang / gcc respectively <br> as the compilers. Supports Python2.7 and Python3.x. |
| 0.0.6<br>October 2017 | Added 'output_directory' argument, auto-compilation <br> will now work if all dependencies are in the source <br> directory, made F77 '.f' files collapse multi-dim <br> arrays to single dimension. |
| 0.0.6<br>October 2017 | Testing sdist distribution instead of wheel. |
| 0.0.6<br>November 2017 | Most recent version. See git for details. |
| 0.0.7<br>November 2017 | Most recent version. See git for details. |


## UPCOMING (checked means in development):

### BUGS

- [ ] Make sure fortran argument names do not conflict with reserved
      words in python, if they do, then add a reserved-word prefix

- [ ] Modules that may be needed are not compiled before the size
      testing program is compiled and executed. This causes crash.

- [ ] If multiple files containing same-named subroutines exist in the
      source fortran file directory then the autocompile_extra_files
      option will cause multiple-declaration linking errors.

### USABILITY

- [ ] Restructure code-base to be broken up into the different parts
      of the wrapping process and unclutter \_\_init__.py in the process.
      (i.e. parse_fortran.py, generate_fortran_wrapper.py,
       generate_cython_wrapper.py, construct_py_module.py)

- [ ] Add example fortran programs that can be used as test cases and
      demonstrations for new users.

### IMPROVEMENTS

- [ ] Add python interface for permanently changing default compilation options.

- [ ] Make intermediate fortran subroutine that passes assumed
      shapes back into C (for passing subroutines as arguments when 
      the subroutines have assumed shape arrays).

- [ ] Add support for fortran data types (structs in C).

- [ ] Add scanner that automatically parses the first few lines of
      python file that imported fmodpy to do automatic compilation.
