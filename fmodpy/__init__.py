# Set up some package-related things.
import os
DIRECTORY = os.path.dirname(os.path.abspath(__file__))
ABOUT_DIR = os.path.join(DIRECTORY, "about")
with open(os.path.join(ABOUT_DIR,"version.txt")) as f:
    __version__ = f.read().strip()
del f

# Import the main configuration (and initialize the configuration).
import fmodpy.config

# Import the main features of this package.
from .fmodpy import fimport, configure, make_python_wrapper, make_python_module

# Set "__all__" so that "from fmodpy import *" returns expected stuff.
__all__ = ["fimport", "configure", "make_python_wrapper", "make_python_module"]
