from .argument import Argument

class Real(Argument):
    type = "REAL"
    c_types = {"8":"ctypes.c_double", "4":"ctypes.c_float"}
    np_types = {"8":"float64", "4":"float32"}
    default_singleton = 1
