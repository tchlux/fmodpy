# Making this module accessible by being called directly from command line.
import os, sys, traceback

# Import all of the fmodpy module
from .fmodpy import __doc__, fimport, configure

if len(sys.argv) < 2:
    import fmodpy
    help(fmodpy)
    exit()
else:
    file_path = os.path.abspath(sys.argv[1])

# Pretty error handling when this file is executed directly     
def custom_excepthook(exc_type, value, tb):
    l = ''.join(traceback.format_exception(exc_type, value, tb))
    print(l)
sys.excepthook = custom_excepthook

# Read command line arguments after the path to the source file.
command_line_args = {}
for arg in sys.argv[2:]:
    if ("=" not in arg):
        from fmodpy.exceptions import UnrecognizedConfiguration
        raise(UnrecognizedConfiguration(
            f"Command line argument {str([arg])[1:-1]} must be formatted"+
            " as '<setting>=<value>', but no '=' found."))
    first_equals = arg.index('=')
    setting, value = arg[:first_equals], arg[first_equals+1:]
    command_line_args[setting] = value

# Call "fimport" providing the given arguments as configurations.
fimport(file_path, **command_line_args)
