from .argument import Argument

class Logical(Argument):
    type = "LOGICAL"
    c_types = {"4":"bint"}
    np_types = {"4":"int32"} # only used for array initialization
    default_singleton = 1

    # Add a warning that 
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        if (self.dimension is not None):
            # Change the C-type of this Logical to be an "int" for arrays.
            self.c_types = self.c_types.copy()
            self.c_types["4"] = "int"
            # Show a warning to the user.
            from fmodpy.config import show_warnings
            if show_warnings:
                import warnings
                warnings.warn("Fortran LOGICAL arrays must be given as 32-big integers.")

