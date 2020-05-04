import importlib
import fmodpy

# code = fmodpy.fimport("code.f90", verbose=True, working_directory=".")
mod_path = fmodpy.build_mod("code.f90", ".", "code", verbose=True)
code = importlib.import_module("code")
arr = code.reserve(1)
print(arr)
del(arr)
