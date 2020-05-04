from .argument import Argument

class Real(Argument):
    type = "REAL"
    c_types = {"8":"double", "4":"float"}
    np_types = {"8":"float64", "4":"float32"}
    default_singleton = 1
