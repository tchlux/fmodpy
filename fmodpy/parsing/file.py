from .code import Code
from . import parse_subroutine, parse_function, parse_module, parse_type

# --------------------------------------------------------------------
# Class for holding all relevant information in a Fortran file.
class Fortran(Code):
    docs = ""
    type = "FILE"
    needs_end = False
    allowed_unknown = True
    can_contain = [(parse_subroutine, "subroutines"),
                   (parse_function, "functions"),
                   (parse_module, "modules"),
                   (parse_type, "types"),
    ]

    # Create Fortran Subroutines with "BIND(C)" that are
    # accessible from C, translate arguments to Fortran, call the
    # Fortran code, and translates arguments back (if necessary).
    def generate_fortran(self):
        lines = ["! This automatically generated Fortran wrapper file allows codes",
                 "! written in Fortran to be called directly from C and translates all",
                 "! C-style arguments into expected Fortran-style arguments (with",
                 "! assumed size, local type declarations, etc.).",
                 ""]
        lines += super().generate_fortran()
        # Return the list of lines for this object (add an extra newline).
        from . import wrap_long_lines
        # Return the final Fortran file as a string.
        return "\n".join(wrap_long_lines(lines+['']))

    # Generate a string representation of the cython code for this wrapper.
    def generate_cython(self):
        lines = ["'''This Cython code is an automatically generated wrapper",
                 "for Fortran code made by 'fmodpy'. The original documentation",
                 "for the Fortran source code follows.",
                 "",
                 self.docs,
                 "'''",
                 '',
                 "import cython",
                 "import numpy",
                 'cimport numpy',
                 'numpy.import_array()',
                 # 'ctypedef numpy.uint8_t uint8'
                 # 'ctypedef numpy.int32_t DTYPE_t',
                 # 'cdef extern from "numpy/arrayobject.h":',
                 # '    void PyArray_ENABLEFLAGS(numpy.ndarray arr, int flags)',
                     '',
                 '# C-function for creating a Numpy array over existing allocated memory.',
                 'cdef ptr_to_numpy_array(numpy.npy_intp N, int t, void * ptr):',
                 '    cdef numpy.ndarray arr = numpy.PyArray_SimpleNewFromData(1, &N, t, ptr)',
                 # '    PyArray_ENABLEFLAGS(arr, numpy.NPY_OWNDATA)',
                     '    return arr',
                 '# --------------------------------------------------------------------',
                 '']
        lines += super().generate_cython()
        # Return the Cython file as a string.
        return "\n".join(lines + [''])
