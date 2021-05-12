from .argument import Argument

class Real(Argument):
    type = "REAL"
    c_types = {"8":"ctypes.c_double", "4":"ctypes.c_float"}
    default_singleton = "1"
