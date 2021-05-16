'''This Python code is an automatically generated wrapper
for Fortran code made by 'fmodpy'. The original documentation
for the Fortran source code follows.

! Test Fortran COMPLEX wrapping and usage from Python with fmodpy.
'''

import os
import ctypes
import numpy

# --------------------------------------------------------------------
#               CONFIGURATION
# 
_verbose = True
_fort_compiler = "gfortran"
_shared_object_name = "test_complex256.so"
_this_directory = os.path.dirname(os.path.abspath(__file__))
_path_to_lib = os.path.join(_this_directory, _shared_object_name)
_compile_options = ['-fPIC', '-shared', '-O3']
_ordered_dependencies = ['test_complex256.f03', 'test_complex256_c_wrapper.f90']
# 
# --------------------------------------------------------------------
#               AUTO-COMPILING
#
# Try to import the existing object. If that fails, recompile and then try.
try:
    clib = ctypes.CDLL(_path_to_lib)
except:
    # Remove the shared object if it exists, because it is faulty.
    if os.path.exists(_shared_object_name):
        os.remove(_shared_object_name)
    # Compile a new shared object.
    _command = " ".join([_fort_compiler] + _compile_options + ["-o", _shared_object_name] + _ordered_dependencies)
    if _verbose:
        print("Running system command with arguments")
        print("  ", _command)
    # Run the compilation command.
    import subprocess
    subprocess.run(_command, shell=True, cwd=_this_directory)
    # Import the shared object file as a C library with ctypes.
    clib = ctypes.CDLL(_path_to_lib)
# --------------------------------------------------------------------


# This defines a C structure that can be used to hold the complex C type.
class c_complex_double(ctypes.Structure):
    _fields_ = [("real", ctypes.c_longdouble), ("imag", ctypes.c_longdouble)]
    # Define an "__init__" that can take a complex number as input.
    def __init__(self, real=0.0, imag=None):
        # If the provided first value was a complex number, expand it out.
        if (hasattr(real, "imag") and hasattr(real, "real") and (imag is None)):
            imag = real.imag
            real = real.real
        # Assign the internal fields.
        self.real = real
        self.imag = imag
    # Define a "value" so that it can be cleanly retreived as a single complex number.
    @property
    def value(self):
        return complex(self.real, self.imag)



# ----------------------------------------------
# Wrapper for the Fortran subroutine TEST_STANDARD

def test_standard(sing_in, array_in, array_out, known_array_out=None, known_matrix_out=None, opt_sing_in=None, opt_sing_out=None):
    '''! Test the basic functionaly of the 'COMPLEX' type and its
! interoperability with Python. This includes, inputs, outputs,
! array inputs with known and unknown size, optional inputs, and
! optional outputs.'''
    
    # Setting up "sing_in"
    if (type(sing_in) is not c_complex_double): sing_in = c_complex_double(sing_in)
    
    # Setting up "sing_out"
    sing_out = c_complex_double()
    
    # Setting up "array_in"
    if ((not issubclass(type(array_in), numpy.ndarray)) or
        (not numpy.asarray(array_in).flags.f_contiguous) or
        (not (array_in.dtype == numpy.dtype(numpy.complex128)))):
        import warnings
        warnings.warn("The provided argument 'array_in' was not an f_contiguous NumPy array of type 'numpy.complex128' (or equivalent). Automatically converting (probably creating a full copy).")
        array_in = numpy.asarray(array_in, dtype=numpy.complex128, order='F')
    array_in_dim_1 = ctypes.c_int(array_in.shape[0])
    
    # Setting up "array_out"
    if ((not issubclass(type(array_out), numpy.ndarray)) or
        (not numpy.asarray(array_out).flags.f_contiguous) or
        (not (array_out.dtype == numpy.dtype(numpy.complex128)))):
        import warnings
        warnings.warn("The provided argument 'array_out' was not an f_contiguous NumPy array of type 'numpy.complex128' (or equivalent). Automatically converting (probably creating a full copy).")
        array_out = numpy.asarray(array_out, dtype=numpy.complex128, order='F')
    array_out_dim_1 = ctypes.c_int(array_out.shape[0])
    
    # Setting up "known_array_out"
    if (known_array_out is None):
        known_array_out = numpy.zeros(shape=(array_out.size), dtype=numpy.complex128, order='F')
    elif ((not issubclass(type(known_array_out), numpy.ndarray)) or
          (not numpy.asarray(known_array_out).flags.f_contiguous) or
          (not (known_array_out.dtype == numpy.dtype(numpy.complex128)))):
        import warnings
        warnings.warn("The provided argument 'known_array_out' was not an f_contiguous NumPy array of type 'numpy.complex128' (or equivalent). Automatically converting (probably creating a full copy).")
        known_array_out = numpy.asarray(known_array_out, dtype=numpy.complex128, order='F')
    known_array_out_dim_1 = ctypes.c_int(known_array_out.shape[0])
    
    # Setting up "known_matrix_out"
    if (known_matrix_out is None):
        known_matrix_out = numpy.zeros(shape=(3, array_out.size), dtype=numpy.complex128, order='F')
    elif ((not issubclass(type(known_matrix_out), numpy.ndarray)) or
          (not numpy.asarray(known_matrix_out).flags.f_contiguous) or
          (not (known_matrix_out.dtype == numpy.dtype(numpy.complex128)))):
        import warnings
        warnings.warn("The provided argument 'known_matrix_out' was not an f_contiguous NumPy array of type 'numpy.complex128' (or equivalent). Automatically converting (probably creating a full copy).")
        known_matrix_out = numpy.asarray(known_matrix_out, dtype=numpy.complex128, order='F')
    known_matrix_out_dim_1 = ctypes.c_int(known_matrix_out.shape[0])
    known_matrix_out_dim_2 = ctypes.c_int(known_matrix_out.shape[1])
    
    # Setting up "opt_sing_in"
    opt_sing_in_present = ctypes.c_bool(True)
    if (opt_sing_in is None):
        opt_sing_in_present = ctypes.c_bool(False)
        opt_sing_in = c_complex_double()
    if (type(opt_sing_in) is not c_complex_double): opt_sing_in = c_complex_double(opt_sing_in)
    
    # Setting up "opt_sing_out"
    opt_sing_out_present = ctypes.c_bool(True)
    if (opt_sing_out is None):
        opt_sing_out_present = ctypes.c_bool(False)
        opt_sing_out = c_complex_double()

    print()
    print("sing_in ", sing_in)
    print("sing_out", sing_out)
    print("array_in ", array_in)
    print("array_out", array_out)

    # Call C-accessible Fortran wrapper.
    clib.c_test_standard(ctypes.byref(sing_in), ctypes.byref(sing_out), ctypes.byref(array_in_dim_1), ctypes.c_void_p(array_in.ctypes.data), ctypes.byref(array_out_dim_1), ctypes.c_void_p(array_out.ctypes.data), ctypes.byref(known_array_out_dim_1), ctypes.c_void_p(known_array_out.ctypes.data), ctypes.byref(known_matrix_out_dim_1), ctypes.byref(known_matrix_out_dim_2), ctypes.c_void_p(known_matrix_out.ctypes.data), ctypes.byref(opt_sing_in_present), ctypes.byref(opt_sing_in), ctypes.byref(opt_sing_out_present), ctypes.byref(opt_sing_out))

    print()
    print("sing_in ", sing_in)
    print("sing_out", sing_out)
    print("array_in ", array_in)
    print("array_out", array_out)

    # Return final results, 'INTENT(OUT)' arguments only.
    return sing_out.value, array_out, known_array_out, known_matrix_out, (opt_sing_out.value if opt_sing_out_present else None)


# ----------------------------------------------
# Wrapper for the Fortran subroutine TEST_EXTENDED

def test_extended(n, opt_array_in=None, known_opt_array_out=None, opt_alloc_array_out=None):
    '''! Test the extended functionaly of the 'COMPLEX' type and its
! interoperability with Python. This includes, optional array
! inputs, optional array outputs, and allocatable array outputs.'''
    
    # Setting up "opt_array_in"
    opt_array_in_present = ctypes.c_bool(True)
    if (opt_array_in is None):
        opt_array_in_present = ctypes.c_bool(False)
        opt_array_in = numpy.zeros(shape=(1), dtype=numpy.complex128, order='F')
    elif (type(opt_array_in) == bool) and (opt_array_in):
        opt_array_in = numpy.zeros(shape=(1), dtype=numpy.complex128, order='F')
    elif ((not issubclass(type(opt_array_in), numpy.ndarray)) or
          (not numpy.asarray(opt_array_in).flags.f_contiguous) or
          (not (opt_array_in.dtype == numpy.dtype(numpy.complex128)))):
        import warnings
        warnings.warn("The provided argument 'opt_array_in' was not an f_contiguous NumPy array of type 'numpy.complex128' (or equivalent). Automatically converting (probably creating a full copy).")
        opt_array_in = numpy.asarray(opt_array_in, dtype=numpy.complex128, order='F')
    if (opt_array_in_present):
        opt_array_in_dim_1 = ctypes.c_int(opt_array_in.shape[0])
    else:
        opt_array_in_dim_1 = ctypes.c_int()
    
    # Setting up "known_opt_array_out"
    known_opt_array_out_present = ctypes.c_bool(True)
    if (known_opt_array_out is None):
        known_opt_array_out_present = ctypes.c_bool(False)
        known_opt_array_out = numpy.zeros(shape=(1), dtype=numpy.complex128, order='F')
    elif (type(known_opt_array_out) == bool) and (known_opt_array_out):
        known_opt_array_out = numpy.zeros(shape=(3), dtype=numpy.complex128, order='F')
    elif ((not issubclass(type(known_opt_array_out), numpy.ndarray)) or
          (not numpy.asarray(known_opt_array_out).flags.f_contiguous) or
          (not (known_opt_array_out.dtype == numpy.dtype(numpy.complex128)))):
        import warnings
        warnings.warn("The provided argument 'known_opt_array_out' was not an f_contiguous NumPy array of type 'numpy.complex128' (or equivalent). Automatically converting (probably creating a full copy).")
        known_opt_array_out = numpy.asarray(known_opt_array_out, dtype=numpy.complex128, order='F')
    if (known_opt_array_out_present):
        known_opt_array_out_dim_1 = ctypes.c_int(known_opt_array_out.shape[0])
    else:
        known_opt_array_out_dim_1 = ctypes.c_int()
    
    # Setting up "opt_alloc_array_out"
    opt_alloc_array_out_present = ctypes.c_bool(True)
    if (opt_alloc_array_out is None):
        opt_alloc_array_out_present = ctypes.c_bool(False)
        opt_alloc_array_out = numpy.zeros(shape=(0), dtype=numpy.complex128, order='F')
    elif (type(opt_alloc_array_out) == bool) and (opt_alloc_array_out):
        opt_alloc_array_out = numpy.zeros(shape=(1), dtype=numpy.complex128, order='F')
    elif ((not issubclass(type(opt_alloc_array_out), numpy.ndarray)) or
          (not numpy.asarray(opt_alloc_array_out).flags.f_contiguous) or
          (not (opt_alloc_array_out.dtype == numpy.dtype(numpy.complex128)))):
        import warnings
        warnings.warn("The provided argument 'opt_alloc_array_out' was not an f_contiguous NumPy array of type 'numpy.complex128' (or equivalent). Automatically converting (probably creating a full copy).")
        opt_alloc_array_out = numpy.asarray(opt_alloc_array_out, dtype=numpy.complex128, order='F')
    if (opt_alloc_array_out_present):
        opt_alloc_array_out_dim_1 = ctypes.c_int(opt_alloc_array_out.shape[0])
    else:
        opt_alloc_array_out_dim_1 = ctypes.c_int()
    opt_alloc_array_out = ctypes.c_void_p(opt_alloc_array_out.ctypes.data)
    
    # Setting up "n"
    if (type(n) is not ctypes.c_int): n = ctypes.c_int(n)
    
    # Setting up "alloc_array_out"
    alloc_array_out = ctypes.c_void_p()
    alloc_array_out_dim_1 = ctypes.c_int()

    # Call C-accessible Fortran wrapper.
    clib.c_test_extended(ctypes.byref(opt_array_in_present), ctypes.byref(opt_array_in_dim_1), ctypes.c_void_p(opt_array_in.ctypes.data), ctypes.byref(known_opt_array_out_present), ctypes.byref(known_opt_array_out_dim_1), ctypes.c_void_p(known_opt_array_out.ctypes.data), ctypes.byref(opt_alloc_array_out_present), ctypes.byref(opt_alloc_array_out_dim_1), ctypes.byref(opt_alloc_array_out), ctypes.byref(n), ctypes.byref(alloc_array_out_dim_1), ctypes.byref(alloc_array_out))

    # Post-processing "opt_alloc_array_out"
    opt_alloc_array_out_size = (opt_alloc_array_out_dim_1.value)
    if (opt_alloc_array_out_present) and (opt_alloc_array_out_size > 0):
        opt_alloc_array_out = numpy.array(ctypes.cast(opt_alloc_array_out, ctypes.POINTER(c_complex_double*opt_alloc_array_out_size)).contents, copy=False).view(numpy.complex128)
    elif (opt_alloc_array_out_size == 0):
        opt_alloc_array_out = numpy.zeros(opt_alloc_array_out_dim_1.value, dtype=numpy.complex128, order='F')
    else:
        opt_alloc_array_out = None
    
    # Post-processing "alloc_array_out"
    alloc_array_out_size = (alloc_array_out_dim_1.value)
    if (alloc_array_out_size > 0):
        alloc_array_out = numpy.array(ctypes.cast(alloc_array_out, ctypes.POINTER(c_complex_double*alloc_array_out_size)).contents, copy=False).view(numpy.complex128)
    elif (alloc_array_out_size == 0):
        alloc_array_out = numpy.zeros(alloc_array_out_dim_1.value, dtype=numpy.complex128, order='F')
    else:
        alloc_array_out = None
    
    # Return final results, 'INTENT(OUT)' arguments only.
    return (known_opt_array_out if known_opt_array_out_present else None), (opt_alloc_array_out if opt_alloc_array_out_present else None), alloc_array_out

