
# cc -fPIC -shared -o libfun.so function.c

# import fmodpy
# simple = fmodpy.fimport("simple.f90", build_dir="fmodpy_simple", verbose=True)


import os

def fimport(f):
    from ctypes import CDLL, byref, c_int, c_long, c_double, c_void_p
    from os import path, system
    name = path.splitext(f)[0]
    system(f"gfortran -fPIC -shared -o {name}.so {f} {name}_bind_c.f90")

    class FortranModule:
        def __init__(self):
            self.name = name
            self.mod = CDLL(name+".so")

        def add(self, a, b, c=0):
            a = c_long(a)
            b = c_long(b)
            c = c_long(c)
            self.mod.c_add(byref(a), byref(b), byref(c))
            return c.value

        def sumall(self, a, b=0):
            a_size_1 = c_int(len(a))
            a = c_void_p(a.ctypes.data)
            b = c_double(b)
            self.mod.c_sumall(byref(a_size_1), a, byref(b))
            return b.value

    return FortranModule()


# simple.c_add.argtypes = [ctypes.c_long]*3

simple = fimport("simple.f90")
a = 3
b = 2
out = simple.add(a, b)
print("out: ", type(out), out)
print("a: ",a)
print("b: ",b)


import numpy as np
a = np.array([0,1,2], dtype=np.float64)
out = simple.sumall(a)
print("out: ", type(out), out)
print("a: ",a)
