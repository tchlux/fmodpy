from .argument import Argument

class Integer(Argument):
    type = "INTEGER"
    c_types = {"8":"long", "4":"int"}
    np_types = {"8":"int64", "4":"int32"}
    default_singleton = 1
