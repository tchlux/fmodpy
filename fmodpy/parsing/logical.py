from .argument import Argument

class Logical(Argument):
    type = "LOGICAL"
    c_types = {"4":"ctypes.c_int", "1":"ctypes.c_bool"}
    default_singleton = "0"

    # For array inputs of logicals, change the C-type to be an "int".
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        if (self.dimension is not None):
            self.c_types = self.c_types.copy()
            self.c_types["4"] = "ctypes.c_int"

    # Add a warning about logical arrays.
    def py_declare(self):
        # Show a warning to the user for passing in logical array types.
        if ((self.dimension is not None) and (self._allowed_input())):
            from fmodpy.config import show_warnings
            if show_warnings:
                import warnings
                warnings.warn("Fortran LOGICAL arrays must be given as 32-bit integers.")
        # Return the usual function.
        return super().py_declare()
        
