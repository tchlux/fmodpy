# Get the version number from the setup file
import os
ABOUT_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)),"about")
with open(os.path.join(ABOUT_DIR,"version.txt")) as f:
    __version__ = f.read().strip()

# Load the module contents from the python file
from .fmodpy import *
