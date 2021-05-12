from .argument import Argument

COMPLEX_STRUCT = """
# This defines a C structure that can be used to hold the complex C type.
class c_complex_{type}(ctypes.Structure):
    _fields_ = [("real", ctypes.c_{type}), ("imag", ctypes.c_{type})]
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
"""

COMPLEX_FLOAT = COMPLEX_STRUCT.format(type="float")
COMPLEX_DOUBLE = COMPLEX_STRUCT.format(type="double")
COMPLEX_LONG_DOUBLE = COMPLEX_STRUCT.format(type="longdouble")

class Complex(Argument):
    type = "COMPLEX"
    default_singleton = "1"
    c_types = {"32":"c_complex_double", "16":"c_complex_double", "8":"c_complex_float"}
    c_types_arrays = {"32":"numpy.complex128", "16":"numpy.complex128", "8":"numpy.complex64"}
    py_types = {"32":COMPLEX_DOUBLE, "16":COMPLEX_DOUBLE, "8":COMPLEX_FLOAT}

    @property
    def py_type(self):
        if (self.size not in self.py_types):
            raise(NotImplementedError(f"\n\nUnrecognized size '{self.size}' for argument '{self.name}', no known corresponding Pythonx type."))
        return self.py_types[self.size]



