# Get the version number from the setup file
import os, sys
# Import the "read" function from the package "setup.py" file.
sys.path = [os.path.dirname(os.path.dirname(os.path.abspath(__file__)))]+sys.path
from setup import read
sys.path = sys.path[1:]

# Read the version from file
__version__ = read("version.txt")[0]

# Load the module contents from the python file
from .fmodpy import *
