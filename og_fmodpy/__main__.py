from __future__ import print_function
# Making this module accessible by being called directly from command line.
import traceback, os, sys, traceback, inspect
# Import all of the fmodpy module
from .fmodpy import __doc__, wrap, \
    USER_MODIFIABLE_GLOBALS, USER_GLOBAL_CASTS

if len(sys.argv) < 2:
    print(__doc__)
    exit()
else:
    file_path = os.path.abspath(sys.argv[1])

#      Pretty error handling when this file is executed directly     
# ===================================================================
def custom_excepthook(exc_type, value, tb):
    l = ''.join(traceback.format_exception(exc_type, value, tb))
    print(l)
sys.excepthook = custom_excepthook

# Create a directory to hold all of the intermediate wrapping file
extra_args = []

# For Python 2.x, do not attempt to generate wrap .
# For Python 3.x, use "inspect.signature".
if sys.version_info >= (3,):
    # Generate a list of modifiable "wrap" parameters
    wrap_parameters = inspect.signature(wrap).parameters
    wrap_parameters = [p for p in wrap_parameters if (not wrap_parameters[p].default)]
    wrap_kwargs = {p:inspect.signature(wrap).parameters[p].default
                   for p in wrap_parameters}
else:
    wrap_parameters = []
    wrap_kwargs = {}

# Extract some of the few acceptable command line arguments
if len(sys.argv) > 2:
    print("RECOGNIZED COMMAND LINE CUSTOMIZATIONS:")
for a in sys.argv[2:]:
    for g_name in USER_MODIFIABLE_GLOBALS + wrap_parameters:
        if ((g_name+"=") in a[:len(g_name+"=")]):
            a = a[len(g_name+"="):]
            if g_name in globals():
                print(g_name,"=",globals()[g_name], end=" -> ")
                param_type = type(globals()[g_name])
                type_cast = USER_GLOBAL_CASTS[param_type]
                globals()[g_name] = type_cast(a)
                print(globals()[g_name])
            elif g_name in wrap_parameters:
                print(g_name,"=",wrap_kwargs[g_name], end=" -> ")
                param_type = type(inspect.signature(wrap).parameters[g_name].default)
                type_cast = USER_GLOBAL_CASTS[param_type]
                wrap_kwargs[g_name] = type_cast(a)
                print(wrap_kwargs[g_name])
            break
    else:
        extra_args.append(a)

# Add extra arguments in as requested functions
if len(extra_args) > 0:
    print("WARNING: fmodpy does not support parameter passing through\n"+
          "         command line for Python2. Consider using code interface.")
    wrap_kwargs["requested_funcs"] = wrap_kwargs["requested_funcs"] + extra_args
    print("requested_funcs = %s -> %s"%([], wrap_kwargs["requested_funcs"]))

# Call "wrap"
wrap(file_path, **wrap_kwargs)

