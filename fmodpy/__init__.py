# Set up some package-related things.
import os
DIRECTORY = os.path.dirname(os.path.abspath(__file__))
ABOUT_DIR = os.path.join(DIRECTORY, "about")
with open(os.path.join(ABOUT_DIR,"version.txt")) as f:
    __version__ = f.read().strip()
del f

# Steal the documentation from .fmodpy, instead of duplicating here.
from .fmodpy import __doc__

# Import the main configuration (needed to initialize the configuration).
import fmodpy.config

# Import the main features of this package.
from .fmodpy import fimport, configure, load_symbol

# Set "__all__" so that "from fmodpy import *" returns expected stuff.
__all__ = ["fimport", "configure", "load_symbol"]

