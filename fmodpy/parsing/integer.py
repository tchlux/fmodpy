from .argument import Argument

class Integer(Argument):
    type = "INTEGER"
    c_types = {"8":"ctypes.c_long", "4":"ctypes.c_int"}
    np_types = {"8":"int64", "4":"int32"}
    default_singleton = 1
