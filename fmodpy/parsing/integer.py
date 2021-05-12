from .argument import Argument

class Integer(Argument):
    type = "INTEGER"
    c_types = {"8":"ctypes.c_long", "4":"ctypes.c_int"}
    default_singleton = "1"
