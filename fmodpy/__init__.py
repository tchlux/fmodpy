# Get the version number from the setup file
import os
ABOUT_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)),"about")
__version__ = open(os.path.join(ABOUT_DIR,"version.txt")).read().strip()

# Load the module contents from the python file
from .fmodpy import *
