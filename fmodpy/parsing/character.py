from .argument import Argument

class Character(Argument):
    type = "CHARACTER"
    c_types = {"1":"ctypes.c_char"}
    default_singleton = "0"

    # Add a warning about logical arrays.
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        if (self.dimension is not None):
            # TODO: Change the C-type of this character be a "uint8" for arrays.
            # self.c_types = self.c_types.copy()
            # self.c_types["1"] = "ctypes.c_char" # Needs to be uint8 in numpy.
            # Show a warning to the user.
            from fmodpy.config import show_warnings
            if show_warnings:
                import warnings
                warnings.warn("Fortran CHARACTER arrays are not supported yet. Raise an issue with an example to encourage development.")
