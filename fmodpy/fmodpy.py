'''
PYTHON USAGE:
 import fmodpy
 fmodpy.wrap("<fortran source file>")
 import <fortran_as_module>


COMMAND LINE USAGE:
  For details on the different options, run python interactively and 
  look at help(fmodpy.wrap). The execution of the program looks like:

    $ python fmodpy.py <fortran source file> [<fmodpy.wrap kwargs>] [<fmodpy.globals() kwargs>] [<functions to wrap>]

  This outputs a <fortran mod name>.so python module that can be
  imported as any other python module would be.
'''

# In case this program is executed in python 2.7
from __future__ import print_function
import re, os, sys, shutil, importlib

# =====================================
#      User customizable variables     
# =====================================

fort_compiler = "gfortran"
fort_compile_arg = "-c"
fort_compiler_options = ["-fPIC", "-O3"]
c_linker = "gcc"
c_compiler = None #"gcc"
module_compile_args = ["-O3"]
module_link_args =    ["-lblas", "-llapack", "-lgfortran"]
module_disallowed_linker_options = ["-Wshorten-64-to-32"]
autocompile_extra_files = True

# ========================
#      fmodpy Globals     
# ========================

# Custom errors that can be raised during the fmodpy process
class NotSupportedError(Exception): pass
class CompileError(Exception):      pass
class FortranError(Exception):      pass
class LinkError(Exception):         pass
class SizeError(Exception):         pass
class NameError(Exception):         pass

# Custom classes for handling lookup errors gracefully
class SizeMap(dict):
    def __missing__(self, f_type):
            raise(NotSupportedError("Type '%s' is not supported."%(f_type)))

class SizeDict(dict):
    def __init__(self, f_type, dictionary):
        self.f_type = f_type
        super(dict, self).__init__()
        self.update(dictionary)
        
    def __missing__(self, size):
        raise(SizeError("Size '%s' is not supported for type '%s'."%(
            size,self.f_type)))

# How to translate a fortran of a given type and size to a c type
FORT_C_SIZE_MAP = SizeMap(
    INTEGER = SizeDict("INTEGER", {
        "4":"int",
        "8":"long"
    }),
    REAL = SizeDict("REAL", {
        "4":"float",
        "8":"double"
    }),
    LOGICAL = SizeDict("LOGICAL", {
        "4":"bint"
    }),
    CHARACTER = SizeDict("CHARACTER", {
        "1":"char"
    }),
    PROCEDURE = SizeDict("PROCEDURE", {
        "":""
    }),
)

# How to translate a fortran of a given type and size to a numpy type
FORT_PY_SIZE_MAP = SizeMap(
    INTEGER = SizeDict("INTEGER", {
        "4":"numpy.int32",
        "8":"numpy.int64"
    }),
    REAL = SizeDict("REAL", {
        "4":"numpy.float32",
        "8":"numpy.float64"
    }),
    LOGICAL = SizeDict("LOGICAL", {
        "4":"numpy.uint32"
    }),
    CHARACTER = SizeDict("CHARATCTER", {
        "1":"numpy.uint8"
    }),
)

# Default sizes (in bytes) of fortran integer and logical types
DEFAULT_F_INT_SIZE = "4"
DEFAULT_F_LOG_SIZE = "4"

# Argument storage (obtained via parsing)
ARG_NAME = "name"
ARG_TYPE = "type"
ARG_SIZE = "size"
ARG_DEFINED = "defined"
ARG_KIND = "kind"
ARG_DIM = "dim"
ARG_INTENT = "intent"
ARG_ALLOCATABLE = "allocatable"
ARG_OPTIONAL = "optional"
ARG_RETURNED = "returned"
ARG_INTERFACE = "interface"
ARG_MESSAGE = "message"

DEFAULT_ARGUMENT = {
    ARG_NAME        :  "UNNAMED",
    ARG_TYPE        :  "N/A" ,
    ARG_SIZE        :  ""    ,
    ARG_DEFINED     :  False ,
    ARG_KIND        :  ""    ,
    ARG_DIM         :  None  ,
    ARG_INTENT      :  ""    ,
    ARG_ALLOCATABLE :  False ,
    ARG_OPTIONAL    :  False ,
    ARG_RETURNED    :  False , # Used for identifying function return values
    ARG_INTERFACE   :  []    , # Used to hold the types of the interface arguments
    ARG_MESSAGE     :  ""    , # Used for displaying comments / warnings
}

def NEW_ARGUMENT():
    arg = DEFAULT_ARGUMENT.copy()
    arg[ARG_DIM] = []
    return arg

# Add spaces to syntax in lines (for easier splitting by white-space)
FORT_TEXT_REPLACEMENTS = {
    "DOUBLE PRECISION": "REAL ( KIND ( 0.0D0 ) )",
    "END DO": "ENDDO",
    "END IF": "ENDIF",
    ",":" , ",
    "(":" ( ",
    ")":" ) ",
    ":":" : ",
    "+":" + ",
    "-":" - ",
    "*":" * ",
    "/":" / ",
    "=":" = ",
}

# Fortran parsing grammar definition
PUBLIC_STATEMENT = "PUBLIC"
PRIVATE_STATEMENT = "PRIVATE"
IMMEDIATELY_EXCLUDE = ["PROGRAM"]
ACCEPTABLE_PREFIXES = ["RECURSIVE", "PURE", "END", "ABSTRACT"]
ACCEPTABLE_LINE_STARTS = ["MODULE", "USE", "SUBROUTINE", "FUNCTION",
                          "INTERFACE"] + [PUBLIC_STATEMENT, PRIVATE_STATEMENT]
ABSTRACT_DECLARATIONS = ["PROCEDURE", "EXTERNAL"]
ACCEPTABLE_DECLARATIONS = sorted(FORT_C_SIZE_MAP.keys()) + ABSTRACT_DECLARATIONS
INTERFACE_START = "INTERFACE"
INTERFACE_END = ["END","INTERFACE"]
# 
# Given the above globals, fmodpy supports the following grammar behaviors:
# 
#   `^IMMEDIATELY_EXCLUDE.*`   
#      -> Assumes fortran has 'main' and cannot be used.
# 
#   `^ACCEPTABLE_PREFIXES*ACCEPTABLE_LINE_STARTS.*`
#      -> Reads as a respective syntax block, starting and ending module
#         / subroutine / function / interface.
# 
#   `^ACCEPTABLE_DECLARATIONS.*`
#      -> Reads as the declaration of a variable, potentially a 
#         subroutine / function argument that needs to be parsed.
# 
#   `^INTERFACE_START[.\n]+^INTERFACE_END`
#      -> Reads interface block with recursion, captures all contents.
# 
#   `^PRIVATE_STATEMENT[::[<name>[,] ]+]*`
#      -> If no list of names follows, assumes entire module is
#         private, otherwise assigns names to an 'ignore' list.
# 
#   `^PUBLIC_STATEMENT[::[<name>[,] ]+]*`
#      -> If no list of names follows, assumes entire module is
#         public, otherwise store names into a list of public functions.
# 

# Functions for identifying the strings before and after the last "."
BEFORE_DOT = lambda name: name[:len(name) - 1 - name[::-1].find(".")];
AFTER_DOT = lambda name: name[min(-name[::-1].find("."),0):]
LEGAL_MODULE_NAME = lambda name: (name.replace("_","")[0].isalpha() and
                                  name.replace("_","")[1:].isalnum() and
                                  (len(name) > 0))
# File related maniplation arguments
CYTHON_EXT = ".pyx"
FMODPY_DIR_PREFIX = "fmodpy_"
FORT_WRAPPER_EXT = "_wrapper.f90"
OLD_PROJECT_PREFIX = "OLD-fmodpy_"
OLD_PROJECT_NAME = lambda name,num: OLD_PROJECT_PREFIX+name+"(%i)"%num
PREPROCESSED_FORTRAN_FILE = "simplified_fortran.f90"

# ======================================
#      Fortran code generation text     
# ======================================

MOD_DOCS = "Module"
C_PREFIX = "C_"
DIM_TYPE = "INTEGER"
PRESENT_TYPE = "LOGICAL"
COPY_IND = "COPY_INDEX_"
FORT_FUNC_OUTPUT_SUFFIX = "_OUTPUT"
FORT_INTERFACE_SUFFIX = "_INTERFACE"

# TODO: Make the fortran generation code only use indents, remove
#       convoluded use of "\n" + indent.
FORT_INDENT = "   "
FORT_LINE_SEPARATOR = "\n" + FORT_INDENT + " "
FORT_USE_MOD_LINE = "USE %s"
FORT_MOD_LINE_BREAK = "\n  "

GET_SIZE_PROG_FILE  = "get_size.f90"
GET_SIZE_EXECUTABLE = "get_size"
GET_SIZE = "PRINT *, '{0}', SIZEOF({0})"
# 0 - The "USE" statement
# 1 - The argument definitions
# 2 - The the body that gets all sizes
GET_SIZE_PROGRAM = """
PROGRAM SIZE_TEST
  {0}
  IMPLICIT NONE
  {1}
  {2}
END PROGRAM SIZE_TEST
"""
# 0 -- source module name
# 1 -- necessary source modules "USE <mod_name>"
# 2 -- abstract interfaces
FORT_WRAPPER_HEADER = """
!     Module generated by fmodpy for fortran file named '{0}'     
!=====================================================================
MODULE {0}_WRAPPER
  ! Any necessary "USE" statements
  {1}
  ! Make sure it is clear that no implicit types are used in this wrapper
  IMPLICIT NONE
  ! Any interfaces defined for "PROCEDURE" declarations
  {2}
CONTAINS
"""
# 0 -- Name of interface
# 1 -- List of argument names
# 2 -- Argument declarations
FORT_WRAPPER_INTERFACE = """
     ! Fortran interface for declaring necessary "PROCEDURE" typed arguments
     SUBROUTINE {0}%s( {1} ) BIND(C)
    {2}
     END SUBROUTINE {0}%s
"""%(FORT_INTERFACE_SUFFIX, FORT_INTERFACE_SUFFIX)
# 0 -- source subroutine name
# 1 -- subroutine arguments in their "c" form
# 2 -- argument declarations
# 3 -- code to run before the call (copy code)
# 4 -- fortran call that resolves necessary values
# 5 -- code to run after the call (copy code)
FORT_WRAPPER_SUB = """
  !     Fortran wrapper for {0}, callable from C     
  !========================================================

  SUBROUTINE c_{0}( {1} ) BIND(c)
    ! This subroutine's only purpose is to call the source fortran
    !  code while also being accessible from C.
    {2}
    ! Preparation code before function call (copying character arrays)
    {3}
    ! Calling source fortran function
    {4}
    ! Copying out any necessary values for return (character arrays)
    {5}
  END SUBROUTINE c_{0}
"""
# 0 - source subroutine name
# 1 - comma separated list of arguments for source subroutine
FORT_SUB_CALL = "CALL {0}({1})"
# 0 - source function name
# 1 - comma separated list of arguments for source function
# 2 - local output storage variable
FORT_FUN_CALL = "{2} = {0}({1})"
# 0 - Copy args and ranges (CHARACTER_COPY_INDEX_0 = 1:<size>, ...)
# 1 - Receiving assignment character
# 2 - Producing assignment character
FORT_COPY_CHAR = """
    ! Must copy character arrays because only characters of len=1 can
    !  be passed directly from C
    DO CONCURRENT ({0})
      {1} = {2}
    END DO
"""
# 0 -- source module name
FORT_WRAPPER_FOOTER = """
END MODULE {0}_WRAPPER
"""

# =======================================
#      C Python code generation text     
# =======================================

# Function to replace the expression "<before>UBOUND(<arg1>,<arg2>)<after>"
# with "<before><arg1>.shape[<arg2>-1]<after>"
def UBOUND_TO_NUMPY(contents):
    start_index = contents.index("UBOUND(") + len("UBOUND(")
    before_call = contents[:start_index-len("UBOUND(")]
    parens = 1
    comma_index = start_index
    for index in range(start_index, len(contents)):
        if contents[index] == ")":
            parens -= 1
        elif contents[index] == "(":
            parens += 1
        elif (parens == 1) and (contents[index] == ","):
            comma_index = index
        if parens <= 0: break
    first_arg = contents[start_index:comma_index]
    second_arg = contents[comma_index+len(","):index]
    after_call = contents[index+len(")"):]
    func_replacement = "{name}.shape[{dim}-1]".format(
        name=first_arg, dim=second_arg).lower()
    return before_call + func_replacement + after_call

# Function to replace the expression "<before>SIZE(<arg>)<after>"
# with "<before><arg>.size<after>"
def SIZE_TO_NUMPY(contents):
    start_index = contents.index("SIZE(") + len("SIZE(")
    before_call = contents[:start_index-len("SIZE(")]
    parens = 1
    for index in range(start_index, len(contents)):
        if contents[index] == ")":
            parens -= 1
        elif contents[index] == "(":
            parens += 1
        if parens <= 0: break
    argument = contents[start_index:index]
    after_call = contents[index+len(")"):]
    if "," not in argument:
        name = argument
        func_replacement = "{name}.size".format(name=name).lower()
    else:
        name, dim = argument.split(",")[:2]
        func_replacement = "{name}.shape[{dim}-1]".format(name=name, dim=dim).lower()
    return before_call + func_replacement + after_call


KNOWN_FORTRAN_FUNCS = {
    "UBOUND" : UBOUND_TO_NUMPY,
    "SIZE"   : SIZE_TO_NUMPY
}

# =========================================
#      Cython Code Generation Settings     
# =========================================

CYTHON_INDENT = "    "
CYTHON_LINE_SEP = "\n" + CYTHON_INDENT
# Statements related to managing optional values
CYTHON_OPT_BOOL = "cdef " + FORT_C_SIZE_MAP["LOGICAL"][DEFAULT_F_LOG_SIZE] \
                 + " {name} = True"
CYTHON_OPTIONAL_CHECK = "if (type({0}) == type(None)):"
CYTHON_OPT_MISSING = "{name} = False"
DEFAULT_NUMPY_ARRAY = "{name} = numpy.ones(shape=({dims}),dtype={type},order='F')"
CYTHON_DEFAULT_VALUE = "{name} = 1"
CYTHON_TYPED_INIT = "cdef {type} {name} = {val}"
# Statements for managing array inputs
CYTHON_MISSING_OPT_ARRAY_SIZE = "1"
CYTHON_ARRAY_CHECK = ("if (not numpy.asarray({0}).flags.f_contiguous):"+
                      CYTHON_LINE_SEP + CYTHON_INDENT +
                      "raise(NotFortranCompatible('Only use numpy arrays"+
                      " that are f_contiguous.'))" + CYTHON_LINE_SEP)
CYTHON_DIM_NAME = "{name}_{dim}"
CYTHON_DIM_DEF = "cdef " + FORT_C_SIZE_MAP["INTEGER"][DEFAULT_F_INT_SIZE] \
                 + " " + CYTHON_DIM_NAME + " = {name}.shape[{dim}]"
BOOL_SUFFIX = "_present"
LOCAL_PREFIX = "local_"
OPTIONAL_SUFFIX = "=None"
CYTHON_HEADER = """
import cython
import numpy

class NotFortranCompatible(Exception): pass
"""

# 0 -- function name
# 1 -- c-function args comma separated
# 2 -- py-function args comma separated
# 3 -- before function call code (declared array dimensions)
# 4 -- "&arg" list of comma separated fortran arguments
# 5 -- after function call code (return statement if necessary)
# 6 -- An interface function that needs to be defined in c
CYTHON_FUNC = """
#      Wrapper for fortran function {0}     
# =================================================

cdef extern:
    void c_{0}( {1} )

@cython.boundscheck(False)
@cython.wraparound(False)
def {0}( {2} ):
    '''{6}'''
    # Prepare for fortran function call (initialize optionals)
    {3}
    # Make fortran function call
    c_{0}({4})
    # Return appropriate values based on "INTENT" from fortran code
    {5}
"""
# 0 -- The name of the argument interface
# 1 -- The list of arguments (c typed) to the interface
# 2 -- The list of local python arguments comma separated
# 3 -- The lines devoted to determining copy-index variables
# 4 -- Copy-in code declaring local python variables
# 5 -- Copy-out code transferring python variable contents back to c variables
C_INTERFACE_FUNC = """
#      Interface for fortran function {0}     
# =================================================

cdef void c_{0}({1}):
    # Recall the python function from global 
    #  (this was assigned by cython during python wrapper call)
    global {0}_global
    # Define any variables needed to copy arrays
    {2}
    # Create local variables that are python types
    {3}
    # Call function with python variables in and out
    {5} = {0}_global({4})
    # Copy out from python variables back into c-typed (fortran) variables
    {6}
"""


# =====================================================
#      Custom subprocess command for compatability     
# =====================================================

# Execute a blocking command with a subprocess, on completion provide
# the return code, stdout as string, and stderr as string. This should
# work across both Python2.7 and Python3.x as well as cross-platform.
#  INPUT:
#   command -- A list of strings or string (space separated) describing
#              a standard command as would be given to subprocess.Popen
# 
#  OUTPUT:
#   return_code -- Straight from the subprocess returncode
#   stdout      -- A list of strings that are the lines of stdout 
#                  produced by the execution of <command>
#   stderr      -- A list of strings that are the lines of stderr
#                  produced by the execution of <command>
def run(command, **popen_kwargs):
    import subprocess
    # For Python 2.x the encoding is a string by default
    # For Python 3.6 and later the encoding can be given as an arguemnt
    if sys.version_info >= (3,6):
        popen_kwargs.update( dict(encoding="UTF-8") )
    proc = subprocess.Popen(command, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, **popen_kwargs)
    stdout, stderr = proc.communicate()
    # Before python 3.6, the encoding had to be handled after collection
    if ((sys.version_info >= (3,)) and (sys.version_info[0] == 3)):
        if (type(stdout) != str): stdout = str(stdout, encoding="UTF-8")
        if (type(stderr) != str): stderr = str(stderr, encoding="UTF-8")
    # Remove windows specific characters and split by the new line
    if stdout: stdout = stdout.replace("\r","").split("\n")
    else:      stdout = ""
    if stderr: stderr = stderr.replace("\r","").split("\n")
    else:      stderr = ""
    # Return the exit code, standard out, and standard error
    return proc.returncode, stdout, stderr


# =================================
#      Processing fortran file     
# =================================

# Function for efficiently performing a series of replacements on a line of text
def CLEAN_TEXT(text, replacements=FORT_TEXT_REPLACEMENTS):
    # Create a proper reg-exp dictionary of replacements
    rep = {re.escape(k):v for (k,v) in replacements.items()}
    # Generate a regular expression pattern with that dictionary
    pattern = re.compile("|".join(rep.keys()))
    # Perform the replacement using python reg-exp search
    return pattern.sub(lambda m: rep[re.escape(m.group(0))], text)

# Given the name of a variable, return the string in fortran that
# represents the type that the fortran compiler will assume
def FORT_IMPLICIT_TYPE(name):
    if len(name) == 0: raise(NameError("Arguments must have a non-zero length name."))
    if name[0] in ["I","J","K","L","M","N"]:
        return "INTEGER"
    else:
        return "REAL"

# Read a fortran file, store it as single lines 
# (without leading and trailing whitespace) and return
def read_fort_file(in_file, old_fortran=False):
    fort_file = []
    curr_line = ""
    for line in in_file:
        if (old_fortran) and (len(line) >0) and (line[0].upper() == "C"):
            line = "!" + line[1:]
        # Split out the comments from the line
        comment_start = line.find("!") if (line.find("!") != -1) else len(line)
        line, comment = line[:comment_start], line[comment_start:].strip()
        # Keep lines that are strictly comments (might be documentation)
        if len(line.strip()) == 0:
            fort_file.append(comment)
            continue
        # Make all fortran upper case
        line = line.upper()
        # Break the line by the colon character
        lines = line.split(";")
        for line in lines:
            if len(line.strip()) == 0: continue
            if (old_fortran):
                line = line[:72]
                # Line continuation character in column 5
                if (len(line) > 5) and (line[5] in ["1","*"]):
                    line = "&" + line[6:]
                    # Retro-actively pop the previous line for continuation
                    if len(curr_line) == 0:
                        curr_line = fort_file.pop(-1)
                # Remove any numeric labels from the front of the line
                line = line.strip().split()
                if (len(line) > 0) and (line[0].isnumeric()):
                    line = line[1:]
                line = " ".join(line)
            # After processing old fortran, properly strip the line of whitespace
            line = line.strip()
            # Remove a leading ampersand if necessary
            if (line[0] == "&"):
                line = line[1:]
            curr_line += line
            if (line[-1] == "&"):
                curr_line = curr_line[:-1]
            else:
                # Process the line into a common format
                #   (upper case, space separated, no commas)
                clean_line = CLEAN_TEXT(curr_line)
                line = [v.strip() for v in clean_line.split()]
                # Only take lines that are acceptable
                acceptable = ("!" in line[0])
                acceptable = acceptable or (line[0] in ACCEPTABLE_PREFIXES)
                acceptable = acceptable or (line[0] in ACCEPTABLE_LINE_STARTS)
                acceptable = acceptable or (line[0] in ACCEPTABLE_DECLARATIONS)
                if line[0] in IMMEDIATELY_EXCLUDE:
                    raise(FortranError(("A valid fortran python module cannot"+
                                        " contain '%s'.")%(line[0])))
                if acceptable:
                    fort_file.append( " ".join(line) )
                curr_line = ""
    return fort_file

# Given a list of text items that makeup of a line of fortran
# code, retreive the type, kind of the type, dimension, and intent. 
# Return a list of arguments (in their dictionary form)
def type_kind_dimension_intent_extra(line, arg_list):
    arg = arg_list[0]
    arg[ARG_DEFINED] = True
    arg[ARG_TYPE] = line[0]
    line = line[1:]
    dim_pre_defined = False
    while (len(line) > 0):
        if ("," in line[0]):
            line = line[1:]
        elif ("(" in line[0]):
            # Found a KIND, LEN, or name of subroutine for PROCEDURE
            kind_string = ""
            open_paren = 1
            i = 0
            for i in range(1,len(line)):
                if ("(" in line[i]):
                    open_paren += line[i].count("(")
                elif (")" in line[i]):
                    open_paren -= line[i].count(")")
                    last = (open_paren == 0)
                    if last: break
                kind_string += line[i]            
            kind_string = kind_string.split(",")
            # Process the character length if necessary, otherwise
            # process the KIND string or LEN string
            for k in kind_string:
                if ("LEN=" in k):
                    arg[ARG_DIM] += [k.replace("LEN=","")]
                elif ("KIND=" in k):
                    arg[ARG_KIND] = k.replace("KIND=","")
                else:
                    arg[ARG_KIND] = k
            line = line[i+1:]
        elif ("DIMENSION" in line[0]):
            dim_pre_defined = True
            # Found a DIMENSION
            dim_string = ""
            open_paren = 0
            for i in range(1,len(line)):
                if ("(" in line[i]):
                    open_paren += line[i].count("(")
                elif (")" in line[i]):
                    open_paren -= line[i].count(")")
                if open_paren == 1:
                    line[i] = line[i].replace(",",";").replace("(","")
                if (open_paren == 0): break
                dim_string += line[i].strip()
            line = line[i+1:]
            arg[ARG_DIM] += dim_string.split(";")
        elif ("INTENT" in line[0]):
            # Found an INTENT
            for i in range(1,len(line)):
                last = (")" in line[i])
                line[i] = line[i].replace("(","").replace(")","")
                arg[ARG_INTENT] += line[i]
                if last: break
            line = line[i+1:]            
        elif ("ALLOCATABLE" in line[0]):
            # Found an ALLOCATABLE
            arg[ARG_ALLOCATABLE] = True
            line = line[1:]
        elif ("OPTIONAL" in line[0]):
            # Found an OPTIONAL
            arg[ARG_OPTIONAL] = True
            line = line[1:]
        else:
            break
    # Remove colon separator
    while (len(line) > 0) and (":" == line[0]): line = line[1:]
    # Process the list of arguments (and their dimensions)
    while len(line) > 0:
        if line[0] == ",":
            # Switching to the next argument
            arg_list.append( arg.copy() )
            arg = arg_list[-1]
            if not dim_pre_defined: arg[ARG_DIM] = []
            line = line[1:]
        elif line[0] == "(":
            # Found a manual dim declaration (revert to behavior as 
            # if the dimension was never defined)
            dim_pre_defined = False
            # Found a dimension declaration
            if arg[ARG_TYPE] in ["CHARACTER"]:
                arg[ARG_DIM] = arg[ARG_DIM][:1]
            else:
                arg[ARG_DIM] = []
            dim_string = ""
            open_paren = 0
            for i in range(len(line)):
                if ("(" in line[i]):
                    open_paren += line[i].count("(")
                elif (")" in line[i]):
                    open_paren -= line[i].count(")")
                if open_paren == 1:
                    line[i] = line[i].replace(",",";").replace("(","")
                if (open_paren == 0): break
                dim_string += line[i].strip()
            line = line[i+1:]
            arg[ARG_DIM] += dim_string.split(";")
        else:
            # Must be a name
            arg[ARG_NAME] = line[0]
            line = line[1:]
    # Returning the input argument list "arg_list"
    return arg_list

# Read through a line-by-line fotran file and find the subroutines,
# functions, arguments, and the types of the arguments
def extract_funcs_and_args(fort_file, requested=[], verbose=False):
    if verbose: print()
    modules = [] # list of module(s), by name, declared in this file,
                 # as well as the modules in "USE" statements
    funcs = {}   # {"<name>":[<argument (dict)>, ...]}
    docs = {MOD_DOCS:[]}  # {"<name>":["<doc line>", ...]}
    in_func = [] # list of the current line's location in functions
    arguments = [] # A list of arguments (in their dictionary form)
    optionals = [] # A list of optional arguments (to search for)
    to_ignore = [] # A list of subroutines to ignore (listed as private)
    is_private = False # Boolean of whether or not this module is globally private
    public_funcs = [] # A list of declared public functions
    interfaces = {}
    in_interface = 0
    interface_lines = []


    # Main loop searching through fortran file
    for line in fort_file:
        #      Capture lines of strictly documentation     
        # =================================================
        if (len(line) > 0) and (line[0] in ["!"]):
            if (len(in_func) == 0) and (len(funcs) == 0):
                # If we are at the beginning of the fortran file (before subroutines)
                docs[MOD_DOCS].append(line)
            elif ( (len(in_func) == 1) and (in_func[-1] in funcs) and
                   not any(a[ARG_DEFINED] for a in funcs[in_func[-1]]) ):
                # If we are in a function and have not yet defined any
                # arguments, then this is likely documentation
                docs[in_func[-1]] = docs.get(in_func[-1],[]) + [line]
            continue
            
        #      Preparing the input line     
        # ==================================
        prefix = []
        split_line = line.split()
        # Strip off acceptable prefixes (because python doesn't care)
        while (len(split_line) > 0) and (split_line[0] in ACCEPTABLE_PREFIXES):
            prefix.append(split_line[0])
            split_line = split_line[1:]

        #      Processing Interfaces     
        # ===============================
        if (((len(prefix) > 0) and (prefix[0] == INTERFACE_END[0])) and
            ((len(split_line) >= 1) and (split_line[0] == INTERFACE_END[1]))):
            in_interface -= 1
            if (in_interface <= 0):
                i_mods, i_funcs, i_ifaces, i_docs = extract_funcs_and_args(interface_lines)
                interface_lines = []
                # Update the list of modules and interfaces
                modules += [m for m in i_mods if m not in modules]
                # Only add interfaces that aren't already defined
                for iface in i_ifaces:
                    if iface not in interfaces: 
                        interfaces[iface] = i_ifaces[iface]
                interfaces.update(i_funcs)
        elif (len(split_line) > 0) and (split_line[0] == INTERFACE_START):
            in_interface += 1
        elif (in_interface > 0): interface_lines.append(line)
        #      Processing Public / Private     
        # =====================================
        elif (len(split_line) > 0) and (split_line[0] == PRIVATE_STATEMENT):
            # Monitor the expression of "PRIVATE" inside a module
            if len(split_line[1:]) == 0:
                is_private = True
            while ":" in split_line: split_line.remove(":")
            while "," in split_line: split_line.remove(",")
            to_ignore += split_line
        elif (len(split_line) > 0) and (split_line[0] == PUBLIC_STATEMENT):
            # Monitor the expression of "PUBLIC" inside a module
            if len(split_line[1:]) == 0:
                is_private = False
            while ":" in split_line: split_line.remove(":")
            while "," in split_line: split_line.remove(",")
            public_funcs += split_line
        #      Processing Module / Subroutine / Function     
        # ===================================================
        elif (any(p in prefix for p in ACCEPTABLE_PREFIXES) or (
                (len(split_line) > 0) and
                any(ls in split_line[0] for ls in ACCEPTABLE_LINE_STARTS))):
            # Process heirarchy syntax in the code, identify modules,
            # subroutines, and functions being declared
            if (len(split_line) > 0) and (split_line[0] in ["MODULE", "USE"]):
                # Add any modules being used to the list (in case they're
                # used in declarations of some of the paramaters)
                mod_name = split_line[1]
                if mod_name not in modules:
                    modules.append(mod_name)
                    if verbose:
                        print("Adding module %s"%mod_name)
                continue
            elif (len(prefix) == 0) or (prefix[0] != "END"):
                # Assuming that this is NOT the end of a subroutine / function
                if (split_line[0] in ["SUBROUTINE", "FUNCTION"]):
                    # We have found a subroutine / function declaration
                    name_end_index = split_line.index("(") if ("(" in split_line) else len(split_line)
                    # Get the name of the subroutine (including prefixes)
                    sub_name = " ".join(prefix + split_line[1:name_end_index])
                    # Initialize storage for this subroutine if it is accessible
                    in_func.append(sub_name)
                    if verbose: print("  "*len(in_func) + "Entering %s"%sub_name)
                    # Skip this subroutine/function if any of:
                    #  - we are in a nested function
                    #  - this is specifically ignored (private)
                    #  - we are private and (not requested) and (not public)
                    #  - there are requests, this is not one
                    if ((len(in_func) > 1) or (sub_name in to_ignore) or
                        (is_private and (sub_name not in requested) 
                         and (sub_name not in public_funcs)) or
                        ((len(requested) > 0) and (sub_name not in requested))):
                        continue
                    # Extract arguments (if they exist)
                    if ("(" in split_line):
                        # Get the names of the arguments (for finding types),
                        # each argument will be (name, type, dimension, intent)
                        line_inside_parens = line[line.index("(")+1:line.index(")")].strip()
                        # Otherwise, get the list of argument names
                        words_in_parens = line_inside_parens.split(",")
                        # Remove blank arguments (if there was a '()')
                        while "" in words_in_parens: words_in_parens.remove("")
                        # Initialize new arguments with names
                        arguments = [NEW_ARGUMENT() for _ in words_in_parens]
                        for arg,name in zip(arguments, words_in_parens):
                            arg.update({ARG_NAME:name.strip()})
                        line_tail = line[line.index(")")+1:]
                    else:
                        arguments = []
                        line_tail = line[name_end_index:]
                    # Convert functions into subroutines via output arguments
                    if (split_line[0] == "FUNCTION"):
                        if ("RESULT" in line_tail):
                            name = line_tail[line_tail.index("(")+1:
                                             line_tail.index(")")]
                        else:
                            name = sub_name
                        new_arg = NEW_ARGUMENT()
                        new_arg.update({ARG_NAME:name.strip(),
                                        ARG_RETURNED:True})
                        arguments.append(new_arg)
                    # Store these arguments
                    funcs[sub_name] = arguments
                    if verbose: 
                        print("  "*len(in_func) + " with arguments: %s"%(
                            [a[ARG_NAME] for a in arguments]))
                    continue
            elif (prefix[0] == "END") and ((len(split_line) == 0) or
                    (split_line[0] in ["SUBROUTINE","FUNCTION"])):
                # If this is the end to something other than a
                # subroutine or function, then skip it. If it just
                # says end, then assume the subroutine or function is ending
                sub_name = in_func.pop(-1)
                if verbose: print("  "*(1+len(in_func)) + " Exiting %s"%sub_name)
                continue
        #      Processing Argument Declarations     
        # ==========================================
        elif (len(split_line) > 0) and (split_line[0] in ACCEPTABLE_DECLARATIONS):
            # Process any variable declarations
            declarations = [NEW_ARGUMENT()]
            # Get the declared arguments
            type_kind_dimension_intent_extra(split_line, declarations)
            for arg in arguments:
                if arg[ARG_DEFINED]: continue
                # Check to see if any declarations match this argumnent
                for dec in declarations:
                    if arg[ARG_NAME] == dec[ARG_NAME]:
                        # Transfer the returned property (in case of function)
                        dec[ARG_RETURNED] = arg[ARG_RETURNED]
                        # Update based on the declared line
                        arg.update(dec)
                        if verbose:
                            print("  "*len(in_func) + " declared '%s'"%(
                                arg[ARG_NAME]))

    # Return the complete list of modules, functions and arguments for
    # each function
    return modules, funcs, interfaces, docs

# Read a fortran file and make it more consistently spaced, also
# reduce all line continuations to single lines for parsing.
def preprocess_fortran_file(in_file, verbose=False, save_dir=""):
    file_name = os.path.basename(in_file.name)
    is_old_fortran = AFTER_DOT(file_name) == "f"
    fort_file = read_fort_file(in_file, is_old_fortran)
    if len(save_dir) > 0:
        # Write the temporary file to file (for debugging)
        translated_fort_file = os.path.join(
            save_dir, PREPROCESSED_FORTRAN_FILE)
        if verbose:
            print("FMODPY: Cleaned input fortran code in '%s'"%translated_fort_file)
        with open(translated_fort_file, "w") as f:
            print(file=f)
            for line in fort_file:
                print(line, file=f)
            print(file=f)     
    return fort_file

# Given a list of arguments, use a compiled fortran program (default
# compiler) to collect the results of "SIZEOF(arg)" for verifying the
# correct level of precision when transferring from python -> fortran
def evaluate_sizes(modules, args, working_dir):
    # First get a list of all modules that need to be included
    module_names = FORT_MOD_LINE_BREAK.join([FORT_USE_MOD_LINE%m
                                             for m in modules[::-1]])
    # Second get a list of the arguments (simplify them) with kinds
    arg_defs = []
    evaluations = []
    for a in args:
        if a[ARG_TYPE] in ABSTRACT_DECLARATIONS: continue
        temp_arg = NEW_ARGUMENT()
        temp_arg[ARG_DEFINED] = True
        temp_arg[ARG_NAME] = a[ARG_NAME]
        temp_arg[ARG_TYPE] = a[ARG_TYPE]
        temp_arg[ARG_KIND] = a[ARG_KIND]
        # Add the definition line and the evaluation line
        arg_defs.append( arg_to_string(temp_arg) )
        evaluations.append( GET_SIZE.format(temp_arg[ARG_NAME]) )
    # Third create a simple kind-testing fortran program
    arg_defs = FORT_LINE_SEPARATOR + FORT_LINE_SEPARATOR.join(arg_defs)
    evaluations = FORT_LINE_SEPARATOR + FORT_LINE_SEPARATOR.join(evaluations)
    prog = GET_SIZE_PROGRAM.format( module_names, arg_defs, evaluations )
    file_name = os.path.join(working_dir, GET_SIZE_PROG_FILE)
    with open(file_name, "w") as f:
        print(prog, file=f)
    # Compile the test program (in the working directory)
    original_dir = os.getcwd()
    os.chdir(working_dir)
    compile_command = [fort_compiler,"-o",GET_SIZE_EXECUTABLE,file_name]
    return_code, stdout, stderr = run(compile_command)
    os.chdir(original_dir)
    # Check for status
    if return_code != 0:
        message = "Failed to compile get size program, received the following:\n\n"
        error_string = "\n".join(stderr)
        # If it was an unexpected error, raise that to user.
        if "Fatal" not in error_string:
            raise(CompileError("Unknwon error in '%s' when resolving types.\n\n%s"%(
                os.path.join(working_dir,GET_SIZE_PROG_FILE), error_string)))
        # This error is likely because of a missing dependency.
        error_string = error_string[error_string.index("Fatal"):].split("\n")[0]
        error_string = error_string.replace(" at (1):",".")
        message += error_string
        raise(CompileError(message))
    # Now run the test_kinds program
    get_size = os.path.join(working_dir,GET_SIZE_EXECUTABLE)
    run_code, stdout, stderr = run([get_size])
    if run_code != 0:
        raise(SizeError("\n"+"\n".join(stderr)))
    for line in stdout:
        line = line.strip().split()
        if len(line) != 2: continue
        arg_name, size = line
        for a in args:
            if (a[ARG_NAME] == arg_name): break
        else:
            raise SizeError("Bad argument name found '%s'."%(arg_name))
        a[ARG_SIZE] = size
    # os.remove(file_name)
    # os.remove(get_size)

# ===============================================
#      Arguments into associated python code     
# ===============================================

# Convert an argument dictionary to a fortran string, expects a
# certain structure of argument dictionary (see top of file)
def arg_to_string(arg, all_args={}):
    # Produce a warning for undefined arguments
    if not arg[ARG_DEFINED]:
        arg_list = ["!!","WARNING","!!",arg[ARG_NAME],"UNDEFINED."]
        return " ".join(arg_list)
    # Process the argument normally (assuming it's defined)
    arg_list = [arg[ARG_TYPE]]
    # Get the list of dimensions (resolving unknown sizes)
    dim_list = arg[ARG_DIM][:]
    # Only add dimension statement if necessary
    if len(dim_list) > 0:
        # Exctract the names of the extra arguments that will be passed
        needed_dims = arg_to_needed_dim(arg, all_args)
        for i in range(len(dim_list)):
            if dim_list[i] in ["*", ":"]:
                dim_list[i] = needed_dims.pop(0)
    # Ignore the kind of characters (because it isn't handled)
    is_character = (arg[ARG_TYPE] in ["CHARACTER"])
    is_procedure = (arg[ARG_TYPE] in ["PROCEDURE"])
    if ((len(arg[ARG_KIND]) > 0) and not is_character):
        kind_string = "" if is_procedure else "KIND="
        arg_list[0] += "(%s%s)"%(kind_string, arg[ARG_KIND])
    elif ((is_character) and (len(dim_list) > 0)):
        # Special handling of character (where len= and kind= share space)
        len_kind = []
        if (len(arg[ARG_KIND]) > 0):
            len_kind.append( "KIND=%s"%(arg[ARG_KIND]) )
        if (len(arg[ARG_DIM]) > 0):
            len_kind.append( "LEN=%s"%(dim_list.pop(0)) )
        if len(len_kind) > 0:
            arg_list[0] += "(" + ",".join(len_kind) + ")"
    if len(dim_list) > 0:
        # Add the dimension line
        dim_string = "DIMENSION("+",".join(["%s"]*len(dim_list))+")"
        arg_list.append(dim_string%tuple(dim_list))
    if len(arg[ARG_INTENT]) > 0:
        arg_list.append("INTENT(%s)"%arg[ARG_INTENT])
    if arg[ARG_ALLOCATABLE]:
        arg_list.append("ALLOCATABLE")
    if arg[ARG_OPTIONAL]:
        arg_list.append("OPTIONAL")
    arg_list[-1] += " :: %s"%arg[ARG_NAME]
    return ", ".join(arg_list)

# Given an argument (and a dictionary of known arguments), return a
# list of the certainly defined values for each dimension (or an empty
# string for uncertain dimension sizes)
def resolve_arg_dimension(arg, all_args={}):
    # Generate a list of resolved dimension values
    arg_dim = [""] * len(arg[ARG_DIM])
    for i in range(len(arg[ARG_DIM])):
        # Skip dimensions that have known behavior
        if arg[ARG_DIM][i] in ["*", ":"]:
            arg_dim[i] = arg[ARG_DIM][i] 
            continue
        # First, check to see if any of the dimensions are known variables
        for a in all_args:
            if arg[ARG_DIM][i] == a[ARG_NAME]:
                arg_dim[i] = a[ARG_NAME]
                break
        else:
            changed = False
            # Second, if the dimension is not a known variable, check
            # to see if there are known fortran functions that can be
            # mapped to numpy functions inside the string
            for k in KNOWN_FORTRAN_FUNCS:
                if (k + "(") in arg[ARG_DIM][i]:
                    arg_dim[i] = KNOWN_FORTRAN_FUNCS[k](arg[ARG_DIM][i])
                    changed = True
            if changed:
                print("WARNING: Processed a dimension of '%s' to be '%s'."
                      %(arg[ARG_NAME], arg_dim[i]))
    return arg_dim

# Given an argument, produce the list of dimension names that need to
# also be defined (extra arguments for the sizes of each dimension)
def arg_to_needed_dim(arg, all_args={}):
    # If this is certainly a C-declared CHARACTER, ignore first dimension
    is_c_char = int((arg[ARG_TYPE] in ["CHARACTER"]) and
                    (arg[ARG_NAME][:len(C_PREFIX)] == C_PREFIX) and
                    (arg[ARG_DIM][0] == 1) )
    needed_dim = []
    for i,d in enumerate(arg[ARG_DIM]):
        # If this is an assumed shape / size array, add extra size arguments
        if d in ["*", ":"]:
            d = CYTHON_DIM_NAME.format(
                name=arg[ARG_NAME][len(C_PREFIX) if is_c_char else 0:],
                dim=i - is_c_char)
            needed_dim.append(d)
    return needed_dim

# Given an argument, return a list containing one string if the
# argument is optional, that string is the optional flag name
def arg_to_is_present(arg, all_args={}):
    if arg[ARG_OPTIONAL]:
        # Add the "is present" boolean argument
        is_present_name = arg[ARG_NAME] + BOOL_SUFFIX
        return [is_present_name]
    else:
        return []

# Return true if the given argument can be optional
def arg_is_optional(arg, all_args={}):
    is_optional = False
    if arg[ARG_OPTIONAL]:
        # If the arg is optional in fortran, it must be optional
        is_optional = True
    elif ((arg[ARG_INTENT] == "OUT") or arg[ARG_RETURNED]):
        # If the argument is output, it can only be optional if the
        # shape is known (because memory space must be allocated)
        is_optional = True
        if len(arg[ARG_DIM]) > 0:
            for d in arg[ARG_DIM]:
                if (d in ["*", ":"]) or (len(d) == 0):
                    is_optional = False
                    break

    return is_optional

# ===============================================
#      Generating fortran code for arguments     
# ===============================================

# Given all arguments, generate the necessary internal copying
# arguments that will be needed to transfer character strings.
def args_to_fort_copy_args(all_args):
    num_needed = 0
    for a in all_args:
        if a[ARG_TYPE] in ["CHARACTER"]:
            num_needed = max(num_needed, len(a[ARG_DIM]))
    # Generate the new arguments (local) for copying characters
    new_args = [NEW_ARGUMENT() for a in range(num_needed)]
    for i,a in enumerate(new_args):
        a[ARG_DEFINED] = True
        a[ARG_NAME] = (COPY_IND + "%i")%(i)
        a[ARG_TYPE] = DIM_TYPE
    return new_args

# Given an argumnent that we know is a character, generate the
# necessary components for creating the DO loop for copying characters
def arg_to_fort_copy_args(arg, all_args={}):
    copy_inds = [a[ARG_NAME] for a in args_to_fort_copy_args([arg])]
    dims = arg_to_needed_dim(arg, all_args)
    dim_names = [ d if d not in ["*", ":"] else dims.pop(0) 
                  for d in arg[ARG_DIM] ]
    # Generate the set of iterators for copying
    iterators = []
    for (ind, size) in zip(copy_inds, dim_names):
        iterators += ["%s = 1:%s"%(ind, size)]
    # Generate the fortran character index string
    char_index_str = arg[ARG_NAME]
    if len(copy_inds) > 1:
        char_index_str += "("+",".join(copy_inds[1:])+")"
    char_index_str += "({0}:{0})".format(copy_inds[0])
    # Generate the C character index string
    char_src_index = C_PREFIX + arg[ARG_NAME]
    char_src_index += "("+",".join(copy_inds)+")"
    return ",".join(iterators), char_index_str, char_src_index

# Given an argument, produce the fortran input names
def arg_to_fort_input(arg, all_args={}):
    inputs = []
    # Add the dimension extras if necessary
    inputs += arg_to_needed_dim(arg, all_args)
    # Add a c prefix to characters
    if arg[ARG_TYPE] in ["CHARACTER"]: prefix = C_PREFIX
    else:                              prefix = ""
    # Add the actual argument itself
    inputs.append( prefix + arg[ARG_NAME] )
    # Add the optional extra if necessary
    inputs += arg_to_is_present(arg,all_args)
    return inputs

# Given an argument, produce the fortran declaration string
def arg_to_fort_declaration(arg, all_args={}):
    declarations = []
    # Add the optional extra if necessary
    for o in arg_to_is_present(arg,all_args):
        o_arg = NEW_ARGUMENT()
        o_arg.update(dict(
            name=o, defined=True, type=PRESENT_TYPE, intent="IN"))
        declarations.append( arg_to_string(o_arg) )
    # Add the dimension extras if necessary
    needed_dims = arg_to_needed_dim(arg, all_args)
    for d in needed_dims:
        d_arg = NEW_ARGUMENT()
        d_arg.update(dict(
            name=d, defined=True, type=DIM_TYPE, intent="IN"))
        declarations.append( arg_to_string(d_arg) )
    # Handle characters (local will be necessary)
    if ((arg[ARG_TYPE] in ["CHARACTER"]) and
        (len(arg[ARG_DIM]) > 0) and (arg[ARG_DIM][0] != 1)):
        temp_arg = arg.copy()
        temp_arg[ARG_NAME] = C_PREFIX + arg[ARG_NAME]
        temp_arg[ARG_DIM] = [1] + temp_arg[ARG_DIM]
        # Add the declaration to the file
        declarations.append( arg_to_string(temp_arg) )
    # Add the declaration of the local character array to file
    declarations.append( arg_to_string(arg) )
    return declarations

# Given an argument, generate a copy if it is needed
def arg_to_fort_copy_in(arg, all_args={}):
    string = ""
    if ( (arg[ARG_TYPE] in ["CHARACTER"]) and (len(arg[ARG_DIM]) > 0) and
         (("IN" in arg[ARG_INTENT]) or (arg[ARG_INTENT] == "")) ):
        iterators, char_index_str, char_src_index = arg_to_fort_copy_args(arg, all_args)
        # Combine all into the copy block
        string += FORT_COPY_CHAR.format(iterators, char_index_str,
                                        char_src_index)
    return string

# Given an argument, generate the argument that will be passed to fortran
def arg_to_fort_call(arg, all_args={}):
    return arg[ARG_NAME]

# Given an argument, generate a copy if it is needed
def arg_to_fort_copy_out(arg, all_args={}):
    string = ""
    if ( (arg[ARG_TYPE] in ["CHARACTER"]) and (len(arg[ARG_DIM]) > 0) and
         (("OUT" in arg[ARG_INTENT]) or (arg[ARG_INTENT] == "")) ):
        iterators, char_index_str, char_src_index = arg_to_fort_copy_args(arg, all_args)
        # Combine all into the copy block
        string += FORT_COPY_CHAR.format(iterators, char_src_index,
                                        char_index_str)
    return string    

# Recursive function for producing branching if statements necessary
# to make the appropriate fortran calls based on provided optionals
def generate_call_code(provided, not_provided, optional,
                       sub_name, arguments, returned):
    if len(optional) == 0:
        # Fill out the call code appropriately for this case,
        # place in the standard arguments.
        call_code = [arg_to_fort_call(a,arguments) for a in arguments
                     if not a[ARG_OPTIONAL]]
        # For the optional arguments, assign them directly
        for a in provided:
            arg = [arg for arg in arguments if arg[ARG_OPTIONAL] and
                   arg_to_is_present(arg)[0] == a][0]
            call_code.append( arg[ARG_NAME]+"="+arg_to_fort_call(arg,arguments) )
        call_code = ", ".join(call_code).upper()
        if len(returned) > 0:
            call_code = FORT_FUN_CALL.format(sub_name, call_code, returned[0][ARG_NAME])
        else:
            call_code = FORT_SUB_CALL.format(sub_name, call_code)
        call_code = FORT_INDENT*(len(provided)+len(not_provided)) + call_code
        return call_code + FORT_LINE_SEPARATOR
    else:
        # Orchestrate recursion, place ".NOT." present variables first
        # assuming it is more likely a user does not want to assign
        # any of the optionals
        code = FORT_INDENT*(len(provided)+len(not_provided))
        code += "IF (%s) THEN"%(optional[0]) + FORT_LINE_SEPARATOR
        code += generate_call_code(provided+[optional[0]],not_provided,
                          optional[1:], sub_name, arguments, returned)
        code += FORT_INDENT*(len(provided)+len(not_provided))
        code += "ELSE" + FORT_LINE_SEPARATOR
        code += generate_call_code(provided,not_provided+[optional[0]],
                          optional[1:], sub_name, arguments, returned)
        code += FORT_INDENT*(len(provided)+len(not_provided))
        code += "ENDIF" + FORT_LINE_SEPARATOR
        return code

# =====================================
#      Python C Related Processing     
# =====================================

# Given an argument, produce the c-python declaration statements
# necessary for the correct values to be prepared for sending to C
def arg_to_py_declaration(arg, all_args={}):
    string = ""
    if arg[ARG_TYPE] in ["PROCEDURE"]:
        string = "global %s_global"%(arg[ARG_NAME]) + CYTHON_LINE_SEP
        string += "{0}_global = {0}".format(arg[ARG_NAME])
    # Adjust the name based on whether or not the argument is optional
    # and add code for initializing the optional value
    local_name = arg[ARG_NAME]
    if arg_is_optional(arg):
        # Define the name, add the default declaration
        local_name = LOCAL_PREFIX + arg[ARG_NAME]
        is_present_name = arg[ARG_NAME] + BOOL_SUFFIX
        if arg[ARG_OPTIONAL]:
            string += CYTHON_OPT_BOOL.format(name=is_present_name) + CYTHON_LINE_SEP
        string += CYTHON_OPTIONAL_CHECK.format(arg[ARG_NAME])
        string += CYTHON_LINE_SEP + CYTHON_INDENT
        if arg[ARG_OPTIONAL]:
            string += CYTHON_OPT_MISSING.format(name=is_present_name)
            string += CYTHON_LINE_SEP + CYTHON_INDENT
        # If this is an optional array, make default initializer
        if len(arg[ARG_DIM]) > 0:
            # Create an optional check that defines this variable
            string += DEFAULT_NUMPY_ARRAY.format(
                name=arg[ARG_NAME], dims=",".join(
                    [CYTHON_MISSING_OPT_ARRAY_SIZE]*len(arg[ARG_DIM])),
                type=FORT_PY_SIZE_MAP[arg[ARG_TYPE]][arg[ARG_SIZE]])
        else:
            string += CYTHON_DEFAULT_VALUE.format(name=arg[ARG_NAME])
        # Add the closing string that assigns the true c-value to the
        # provided optional value
        string += CYTHON_LINE_SEP
        arg_c_type = FORT_C_SIZE_MAP[arg[ARG_TYPE]][arg[ARG_SIZE]]
        if len(arg[ARG_DIM]) > 0:
            arg_c_type += "["+",".join([":"]*len(arg[ARG_DIM]))+"]"
        string += CYTHON_TYPED_INIT.format(
            type=arg_c_type, name=local_name, val=arg[ARG_NAME])
        string += CYTHON_LINE_SEP
    # If this argument has dimensions that need to have c-values, then
    # define those c values as well
    dim_args = arg_to_needed_dim(arg)
    if len(dim_args) > 0:
        string += CYTHON_ARRAY_CHECK.format(local_name)
    for dname in dim_args:
        i = dname.split("_")[-1]
        string += CYTHON_DIM_DEF.format(
            name=arg[ARG_NAME], dim=i) + CYTHON_LINE_SEP
    # Return the string of the cython code to be inserted
    return string + CYTHON_LINE_SEP

# Given an argument, produce the c-python function input string
def arg_to_py_input(arg, all_args={}):
    if arg_is_optional(arg):
        string = "%s" + OPTIONAL_SUFFIX
    elif arg[ARG_TYPE] in ["PROCEDURE"]:
        string = "%s"
    else:
        string = FORT_C_SIZE_MAP[arg[ARG_TYPE]][arg[ARG_SIZE]]
        if len(arg[ARG_DIM]) > 0:
            string += "["+ ",".join([":"]*len(arg[ARG_DIM])) +"]"
        string += " %s"
    return string%(arg[ARG_NAME])

# Given an argument, produce the c-python function output string
def arg_to_py_output(arg, all_args={}):
    if arg[ARG_TYPE] in ["PROCEDURE"]:
        string = ""
    else:
        # Get the local name that will have been passed to the fortran function
        if arg_is_optional(arg):
            local_name = LOCAL_PREFIX + arg[ARG_NAME]
        else:
            local_name = arg[ARG_NAME]
        # Handle dimensioned and non-dimensioned arguments differently
        if len(arg[ARG_DIM]) > 0:
            string = "numpy.asarray(%s, order='F')"%(local_name)
        else:
            string = "%s"%(local_name)
    return string

# Given an argument, produce the c-python function call string
def arg_to_py_c_call(arg, all_args={}):
    string = ""
    # Add the dimension extras if necessary
    for d in arg_to_needed_dim(arg, all_args): string += "&%s, "%(d)
    # Get the name of the argument (that can be passed to py_c_func)
    # based on the local name (if it's optional it'll be different)
    if arg_is_optional(arg):
        local_name = LOCAL_PREFIX + arg[ARG_NAME]
    else:
        local_name = arg[ARG_NAME]
    if arg[ARG_TYPE] in ["PROCEDURE"]:
        prefix = "&c_"
    else:
        prefix = "&"
    suffix = "[0]"*len(arg[ARG_DIM])
    # Add the actual argument itself
    string += prefix + local_name + suffix
    # Add the optional extra if necessary
    for o in arg_to_is_present(arg,all_args): string += ", &" + o
    return string

# Given an argument, produce the c fortran call version of the argument
def arg_to_c_fort_call(arg, all_args={}):
    string = ""
    # Add the dimension extras if necessary
    for d in arg_to_needed_dim(arg, all_args): 
        string += FORT_C_SIZE_MAP["INTEGER"][DEFAULT_F_INT_SIZE]
        string += "* %s, "%(d)
    if arg[ARG_TYPE] in ["PROCEDURE"]:
        # Generate the procedure definnition string
        arguments = ", ".join([FORT_C_SIZE_MAP[a[ARG_TYPE]][a[ARG_SIZE]]+"*"
                               for a in arg[ARG_INTERFACE]])
        string += "void (*%s)(%s)"%(arg[ARG_NAME], arguments)
    else:
        # Add the actual argument itself
        string += FORT_C_SIZE_MAP[arg[ARG_TYPE]][arg[ARG_SIZE]] + "* " + arg[ARG_NAME]
    # Add the optional extra if necessary
    for o in arg_to_is_present(arg,all_args): 
        string += ", "
        string += FORT_C_SIZE_MAP["LOGICAL"][DEFAULT_F_LOG_SIZE]
        string += "* " + o
    return string    

# Given all arguments, generate the necessary internal copying
# arguments that will be needed to transfer character strings.
def args_to_c_copy_args(all_args):
    num_needed = 0
    for a in all_args:
        num_needed = max(num_needed, len(a[ARG_DIM]))
    # Generate the new arguments (local) for copying characters
    new_args = [NEW_ARGUMENT() for a in range(num_needed)]
    for i,a in enumerate(new_args):
        a[ARG_DEFINED] = True
        a[ARG_NAME] = (COPY_IND + "%i")%(i)
        a[ARG_TYPE] = FORT_C_SIZE_MAP[DIM_TYPE][DEFAULT_F_INT_SIZE]
    return new_args


# Given an argument, produce a list of the lines necessary for
# copying the value of a python variable back out
def arg_to_interface_copy(arg, out=False):
    lines = []
    if len(arg[ARG_DIM]) > 0:
        if not out:
            dimension = ",".join(["%s[0]"%(d) for d in arg[ARG_DIM]])
            line = LOCAL_PREFIX+"{name} = numpy.ones({dimension}, dtype={type})".format(
                name=arg[ARG_NAME], dimension=dimension,
                type=FORT_PY_SIZE_MAP[arg[ARG_TYPE]][arg[ARG_SIZE]])
            lines.append(line)

        # Generate the for loops necessary to copy arrays
        copy_indices = []
        for i in range(len(arg[ARG_DIM])):
            iterator = (COPY_IND + "%i")%(i)
            copy_indices.append( iterator )
            line = i*CYTHON_INDENT + "for {iterator} in range({steps}[0]):".format(
                iterator=iterator, steps=arg[ARG_DIM][i])
            lines.append(line)
        # Generate the expression needed to copy indices (fortran
        # contiguous style) into the local python numpy array.
        index_map = []
        for i in range(len(arg[ARG_DIM])):
            if i > 0:
                first = "*".join([d+"[0]" for d in arg[ARG_DIM]]) + "*"
            else: first = ""
            second = copy_indices[i]
            index_map.append( first+second )
        fort_index = " + ".join(index_map)
        py_index = ", ".join(copy_indices)
        # Depending on which way we want to copy
        if out:
            copy_string = "{name}[{fort_index}] = local_{name}[{py_index}]"
        else:
            copy_string = "local_{name}[{py_index}] = {name}[{fort_index}]"
        line = CYTHON_INDENT*(i+1) + copy_string.format(
                name=arg[ARG_NAME], py_index=py_index, fort_index=fort_index)
        lines.append(line)
    return lines

# From an interface, generate the c / python function that can be used
# to communicate between fortran and python
def interface_to_c_func(name, arguments):
    # Generate the copy in code
    python_copy_in = []
    for a in arguments:
        python_copy_in += arg_to_interface_copy(a, out=False)

    # Generate the lists of arguments that will be sent into the
    # function and those that will be returned by the function
    python_func_inputs = []
    python_func_outputs = []
    for a in arguments:
        if (len(a[ARG_INTENT]) == 0) or ("IN" in a[ARG_INTENT]):
            if len(a[ARG_DIM]) > 0:
                python_func_inputs.append(LOCAL_PREFIX + a[ARG_NAME])
            else:
                python_func_inputs.append(a[ARG_NAME] + "[0]")
        if (len(a[ARG_INTENT]) == 0) or ("OUT" in a[ARG_INTENT]):
            if len(a[ARG_DIM]) > 0:
                python_func_outputs.append(LOCAL_PREFIX + a[ARG_NAME])
            else:
                python_func_outputs.append(a[ARG_NAME] + "[0]")

    # Generate the copy out code
    python_copy_out = []
    for a in arguments:
        python_copy_out += arg_to_interface_copy(a, out=True)

    # Convert all the generated lists into appropriate code blocks as strings
    c_arguments = ", ".join([FORT_C_SIZE_MAP[a[ARG_TYPE]][a[ARG_SIZE]]+"* "+
                             a[ARG_NAME] for a in arguments]).lower()
    copy_declarations = args_to_c_copy_args(arguments)
    copy_declarations = CYTHON_LINE_SEP.join(["cdef int " + a[ARG_NAME]
                                              for a in copy_declarations]).lower()
    python_copy_in = CYTHON_LINE_SEP.join(python_copy_in).lower()
    python_func_inputs = ", ".join(python_func_inputs).lower()
    python_func_outputs = ", ".join(python_func_outputs).lower()
    python_copy_out = CYTHON_LINE_SEP.join(python_copy_out).lower()

    # 0 -- The name of the argument interface
    # 1 -- The list of arguments (c typed) to the interface
    # 2 -- The list of local python arguments comma separated
    # 3 -- The lines devoted to determining copy-index variables
    # 4 -- Copy-in code declaring local python variables
    # 5 -- Copy-out code transferring python variable contents back to c variables
    return C_INTERFACE_FUNC.format(name.lower(), c_arguments, copy_declarations,
                                   python_copy_in, python_func_inputs,
                                   python_func_outputs, python_copy_out)

# ====================================================
#      Code for Generating the Intermediate Files     
# ====================================================

# Generate the fortran wrapper code (in fortran) that can be called by c
def generate_fort_c_wrapper(modules, project_name, funcs_and_args, interfaces):
    fort_c_wrapper = ""
    # Generate the sting of "USE <MOD>"
    module_names = FORT_MOD_LINE_BREAK.join([FORT_USE_MOD_LINE%m
                                             for m in modules[::-1]])
    # Add interfaces if necessary
    interface_code = ""
    if len(interfaces) > 0:
        interface_code += FORT_LINE_SEPARATOR + "\n  ABSTRACT INTERFACE"
        for i_name in interfaces:
            args = ", ".join([a[ARG_NAME] for a in interfaces[i_name]])
            decs = (FORT_LINE_SEPARATOR).join(
                [FORT_INDENT + arg_to_string(a) for a in interfaces[i_name]])
            interface_code += FORT_WRAPPER_INTERFACE.format(i_name, args, decs)
        interface_code += "  END INTERFACE" + FORT_LINE_SEPARATOR * 2

    fort_c_wrapper += FORT_WRAPPER_HEADER.format(
        project_name.upper(), module_names, interface_code).strip()


    for sub_name in funcs_and_args:
        # Get the arguments
        arguments = funcs_and_args[sub_name]
        # Split up the arguments into those that are returned and not
        returned = [a for a in arguments if a[ARG_RETURNED]]
        arguments = [a for a in arguments if not a[ARG_RETURNED]]
        if len(returned) > 0:
            if len(returned) > 1:
                raise("ERROR:    Did not expect multiple returns..")
        in_arg_names = []
        arg_defs = []
        for a in arguments + returned:
            in_arg_names += arg_to_fort_input(a,arguments)
            arg_defs += arg_to_fort_declaration(a,arguments)
        copy_args = args_to_fort_copy_args(arguments+returned)
        arg_defs += [arg_to_string(a) for a in copy_args]
        copy_in_code = [arg_to_fort_copy_in(a,arguments) for a in arguments]
        copy_out_code = [arg_to_fort_copy_out(a,arguments) for a in arguments]
        # Clean up each of these, removing empty elements
        for str_list in [in_arg_names, arg_defs, copy_in_code, copy_out_code]:
            while "" in str_list: str_list.remove("")
        # Convert these into single strings
        in_arg_names = ", ".join(in_arg_names).upper()
        arg_defs = FORT_LINE_SEPARATOR.join(arg_defs).upper()
        copy_in_code = FORT_LINE_SEPARATOR.join(copy_in_code).upper()
        copy_out_code = FORT_LINE_SEPARATOR.join(copy_out_code).upper()

        # Separate arguments out into those that are optional and mandatory
        optionals = [arg_to_is_present(a)[0] for a in arguments if a[ARG_OPTIONAL]]
        call_code = generate_call_code([],[],optionals,sub_name,
                                       arguments,returned).upper()
        call_code = call_code[:-len(FORT_LINE_SEPARATOR)]

        # Compile all the above snippets of code into one subroutine
        fort_code = FORT_WRAPPER_SUB.format(sub_name, in_arg_names,
                                            arg_defs, copy_in_code,
                                            call_code, copy_out_code).split("\n")
        fort_code.append("")

        # Make sure that line wraps occur where necessary
        line = 0
        indent_size = 0
        while (line < len(fort_code)):
            # If this is a newly continued line, update the indent size
            if (len(fort_code[line-1]) > 0 ) and (fort_code[line-1][-1] != "&"):
                indent_size = (5 + len(fort_code[line]) -
                               len(fort_code[line].lstrip()))
            line_str = fort_code[line].split("!")
            line_str, comment = (line_str[0], line_str[1]
                                 if len(line_str) > 1 else "")
            line += 1
            if len(line_str) > 70:
                line_str, rest = (line_str[:70] + "&",
                                  " "*indent_size+"&"+line_str[70:])
                # Update the truncated line
                fort_code[line - 1] = line_str
                # Add the new line (will be processed later)
                fort_code.insert(line, rest)
                continue
            if len(comment) > 0: line_str += " !"+comment

        fort_c_wrapper += "\n".join(fort_code)

    fort_c_wrapper += FORT_WRAPPER_FOOTER.format(project_name.upper()).strip()

    return fort_c_wrapper

# Generate the c wrapper code (in c-python) that can be called from python
def generate_c_python_wrapper(modules, project_name, funcs_and_args,
                              interfaces, documentation):

    c_python_wrapper = "'''%s'''"%("\n".join(documentation[MOD_DOCS])) + "\n\n"

    c_python_wrapper += CYTHON_HEADER

    # Define all of the interfaces necessary
    for i_face in interfaces:
        c_python_wrapper += interface_to_c_func(i_face, interfaces[i_face])

    for func_name in funcs_and_args:
        arguments = funcs_and_args[func_name]
        # Lower case everything on the python and c side of things
        func_name = func_name.lower()
        for arg in arguments:
            arg[ARG_NAME] = arg[ARG_NAME].lower()
            # This next line gets the correct set of dimensions
            arg[ARG_DIM] = resolve_arg_dimension(arg, arguments)
            arg[ARG_DIM] = [v.lower() for v in arg[ARG_DIM]]

        c_func_args = [arg_to_c_fort_call(a,arguments) for a in arguments]
        py_func_args = [arg_to_py_input(a,arguments)   for a in arguments]
        py_prep_code = [arg_to_py_declaration(a, arguments) for a in arguments]
        py_c_call = [arg_to_py_c_call(a,arguments)     for a in arguments]
        # Outputs are given by intent if it is not a function, if it
        # is a function then the retuned value is the only value to return
        for a in arguments:
            if a[ARG_RETURNED]:
                py_output = [ arg_to_py_output(a,arguments) ]
                break
        else:
            py_output = [ arg_to_py_output(a,arguments)    for a in arguments
                          if (a[ARG_INTENT] == "") or ("OUT" in a[ARG_INTENT]) ]

        # Clean up each of these, removing empty elements
        for str_list in [c_func_args, py_func_args, py_prep_code,
                         py_c_call, py_output]:
            to_remove = []
            for v in str_list:
                if len(v.strip()) == 0:
                    to_remove.append(v)
            for v in to_remove:
                str_list.remove(v)

        c_func_args = ", ".join(c_func_args)
        py_func_args = ", ".join(py_func_args)
        py_prep_code = "".join(py_prep_code)
        py_c_call = ", ".join(py_c_call)
        py_output = ", ".join(py_output)

        py_output = CYTHON_LINE_SEP + "return " + py_output
        
        code = CYTHON_FUNC.format(func_name, c_func_args, py_func_args,
                                  py_prep_code, py_c_call, py_output, 
                                  CYTHON_LINE_SEP.join(documentation.get(func_name.upper(), [])))
        c_python_wrapper += code + "\n\n"

    return c_python_wrapper

# =====================================================
#      Master fmodpy Function for Wrapping Fortran     
# =====================================================

# Given the path to the file that we are creating an extension for,
# create and prepare a working directory for the project compilation
def prepare_working_directory(source_file, source_dir, project_name,
                              working_direcotry="", verbose=False):
    if len(working_direcotry) == 0:
        working_dir = os.path.join(source_dir, FMODPY_DIR_PREFIX+project_name)
    else:
        working_dir = os.path.abspath(os.path.expanduser(working_direcotry))

    # Generate the names of files that will be created by this
    # program, so that we can copy them to an "old" directory if necessary.
    fort_wrapper_file = project_name+FORT_WRAPPER_EXT
    cython_file = project_name+CYTHON_EXT

    if verbose:
        print()
        print("="*60)
        print("Input file directory:",source_dir)
        print("Input file name:     ",source_file)
        print("Base module name:    ",project_name)
        print("Using working dir:   ",working_dir)
        print("  fortran wrapper:   ",fort_wrapper_file)
        print("  cython file:       ",cython_file)
        print("="*60)
        print()

    #      Prepare a working directory     
    # =====================================
    if not os.path.exists(working_dir):
        # Otherwise, create the initial working directory
        os.makedirs(working_dir)
    elif len(working_direcotry) == 0:
        # Otherwise the user didn't give a directory, but it exists
        # for some reason (probably failure?) So save contents.
        # 
        # Identify a viable name for the old project contents
        num = 1
        while os.path.exists( os.path.join(
                working_dir, OLD_PROJECT_NAME(project_name, num))): num += 1
        old_proj_name = OLD_PROJECT_NAME(project_name, num)
        # Create the directory for old project contents
        old_proj_dir = os.path.join(working_dir,old_proj_name)
        os.makedirs(os.path.join(working_dir,old_proj_name))
        # Move the files that would've been created by this project
        # before into an "OLD" directory
        for f in os.listdir(working_dir):
            f = os.path.join(working_dir, f)
            if not os.path.isdir(f):
                shutil.move(f,os.path.join(old_proj_dir,os.path.basename(f)))
        if verbose:
            print("FMODPY:  Moved existing working directory contents to\n  '%s'"%(
                os.path.join(working_dir,old_proj_name) ))

        

    # If the working directory is not the same as the file directory,
    # copy all the contents of the file directory into the working
    # directory (in case any of them are used by the fortran project)
    if source_dir != working_dir:
        for f in os.listdir(source_dir):
            source = os.path.join(source_dir,f)
            if not os.path.isdir(source):
                destination = os.path.join(working_dir,f)
                if verbose:
                    print("FMODPY: Copying '%s' to '%s'"%(source, destination))
                shutil.copyfile(source, destination)
    
    # Return the name of the project, the three files that will be
    # created by fmodpy, and the working directory to do this in
    return working_dir

# Master function for orchestrating the creation of a python module
# from a fortran file. All of the SUBROUTINE and FUNCTION in the
# fortran file will be available in a standard python module fashion,
# with the same name as the fortran file.
def fortran_to_python(fortran_file_path, working_dir, project_name,
                      requested_funcs=None, verbose=True):
    if (requested_funcs) == type(None):
        requested_funcs = []
    # Convert the list of requested funcs to all upper case for consistency
    for i in range(len(requested_funcs)): 
        requested_funcs[i] = requested_funcs[i].upper()

    # Open the input code as a file
    in_file = open(fortran_file_path, "r")
    # Get a cleaned preprocessed version of the input file
    fort_file = preprocess_fortran_file(in_file, verbose=verbose, save_dir=working_dir)
    # Capture a boolean to know if it's old fortran or modern fortran
    is_old_fortran = AFTER_DOT(os.path.basename(fortran_file_path)) == "f"
    # Identify functions and arguments     
    modules, funcs_and_args, interfaces, documentation = (
        extract_funcs_and_args(fort_file, requested_funcs) )
    if len(funcs_and_args) == 0:
        raise(FortranError("Did not find any fortran subroutines or functions to wrap."))

    # If the user requested specific functions, only consider those
    if len(requested_funcs) > 0:
        to_remove = [f for f in funcs_and_args if f not in requested_funcs]
        for f in to_remove: funcs_and_args.pop(f)

    #      Clean up the set of functions     
    # =======================================
    used_interfaces = []
    # Define arguments using implicit types, un-define arguments that
    # are of unrecognized types.
    for func in funcs_and_args:
        for arg in funcs_and_args[func]:
            if (not arg[ARG_DEFINED]):
                arg[ARG_DEFINED] = True
                arg[ARG_TYPE] = FORT_IMPLICIT_TYPE(arg[ARG_NAME])
                arg[ARG_MESSAGE] = "  !! Warning !! Not defined, using implicit type '%s'."%(arg[ARG_TYPE])
            # Remove intent from ARG_RETURNED arguments for functions
            if arg[ARG_RETURNED]: 
                arg[ARG_NAME] = arg[ARG_NAME] + FORT_FUNC_OUTPUT_SUFFIX
                arg[ARG_INTENT] = ""
            # Remove dimensions from arrays for old fortran (because
            # old fortran doesn't have assumed shape)
            if (is_old_fortran and (len(arg[ARG_DIM]) > 0)):
                arg[ARG_DIM] = ["*"]
            # Try to resolve "external" declarations
            if arg[ARG_TYPE] in ["EXTERNAL"]:
                if (arg[ARG_NAME] in interfaces):
                    arg[ARG_TYPE] = "PROCEDURE"
                    arg[ARG_KIND] = arg[ARG_NAME] + FORT_INTERFACE_SUFFIX
                    arg[ARG_INTERFACE] = interfaces[arg[ARG_NAME]]
                    # Check to make sure the interface is legal
                    for i_arg in interfaces[func]:
                        message = ""
                        for d in i_arg[ARG_DIM]:
                            if d in ["*", ":"]:
                                raise(NotSupportedError((
                                    "External procedure '%s' cannot take assumed"+
                                    " shape / size array '%s' as an argument.\n "+
                                    "Consider passing size as extra argument.")%(
                                        func,i_arg[ARG_NAME])))
                        if (not i_arg[ARG_DEFINED]) and (func not in to_remove):
                            raise(FortranError(
                                "Could not locate argument definition for "+
                                "'%s' in interface '%s'."%(i_arg[ARG_NAME], func)))
                    # Track which interfaces are used (and should be wrapped)
                    if arg[ARG_NAME] not in used_interfaces:
                        used_interfaces
                else:
                    raise(FortranError(
                        ("External procedure argument '%s' in '%s' does not "+
                         "have a defined interface.")%(arg[ARG_NAME], func)))
            # Un-define arguments of unacceptable types
            if arg[ARG_TYPE] not in ACCEPTABLE_DECLARATIONS:
                raise(NotSupportedError(
                    "Not able to process argument of type "+
                    "'%s' in '%s'."%(arg[ARG_TYPE], func)))
            # Print out notices for bad functions
            if (not arg[ARG_DEFINED]) and (func not in to_remove):
                raise(FortranError(
                    "Could not locate argument definition for "+
                    "'%s' argument in '%s'."%(arg[ARG_NAME], func)))

    # Reduce the set of interfaces to those which are usable from python
    interfaces = {iface:interfaces[iface] for iface in used_interfaces}

    #      Print out the set of functions     
    # ========================================
    if verbose:
        if len(requested_funcs) > 0:
            print("FMODPY:  Only producing wrappers for:\n  %s"%requested_funcs)
        if len(modules) > 0:
            print("FMODPY:  Using modules:\n  %s\n"%modules)
        for func in funcs_and_args:
            print("\n",func+":")
            for arg in funcs_and_args[func]:
                print("  %s"%arg_to_string(arg) + arg[ARG_MESSAGE])
            # # Print out documentation as well
            # doc = documentation.get(func,[])
            # if len(doc) > 0:
            #     for line in doc: print("",line)
        print()

    # ==============================================
    #       Automatically Compile Extra Files       
    #      (in case some are necessary modules)     
    # ==============================================
    if autocompile_extra_files:
        # Store original directory to revert back after compilation
        original_dir = os.getcwd()
        os.chdir(working_dir)

        # Try and compile the rest of the files (that might be fortran) in
        # the working directory in case any are needed for linking.
        should_compile = []
        # Generate the list of files that we sould try to autocompile
        for f in os.listdir():
            f = f.strip()
            # Skip the preprocessed file, the size program, and directories
            if ( (PREPROCESSED_FORTRAN_FILE in f) or
                 (GET_SIZE_PROG_FILE in f) or
                 (os.path.isdir(f)) or
                 ("f" not in AFTER_DOT(f)) ):
                continue
            # Make sure the file does not have any immediate exclusions,
            # if it does then skip it
            with open(f) as fort_file:
                exclude_this_file = False
                # Read through the file, look for exclusions
                for line in fort_file.readlines():
                    line = line.strip().upper().split()
                    if (len(line) > 0) and (line[0] in IMMEDIATELY_EXCLUDE):
                        exclude_this_file = True
                        break
                if exclude_this_file:
                    if verbose:
                        print(("FMODPY: Skipping '%s' because it contains "+
                               "one of %s.")%(f, IMMEDIATELY_EXCLUDE))
                    continue
            should_compile.append(f)
        # Handle dependencies by doing rounds of compilation, presuming
        # only files with fewest dependencies will compile first
        successes = [None]
        # Continue rounds until (everything compiled) or (no success)
        while (len(should_compile) > 0) and (len(successes) > 0):
            successes = []
            for f in should_compile:
                # Try to compile all files that have "f" in the extension
                if verbose: print("FMODPY: Compiling '%s'..."%(f))
                code, stdout, stderr = run([fort_compiler,fort_compile_arg]+fort_compiler_options+[f])
                if code == 0: successes.append(f)
            # Remove the files that were successfully compiled from
            # the list of "should_compile"
            for f in successes:
                should_compile.remove(f)
        # Revert to the original directory
        os.chdir(original_dir)

    if verbose:
        print("FMODPY: Evaluating byte-sizes of fortran arguments...")

    # =====================================================
    #      Evaluate the size (in bytes) of each of the     
    # arguments to determine the appropriate C types to use     
    # =====================================================
    for func in funcs_and_args:
        evaluate_sizes(modules, funcs_and_args[func], working_dir)
    for func in interfaces:
        evaluate_sizes(modules, interfaces[func], working_dir)

    if verbose:
        print("FMODPY: Generating the c-compatible fortran wrapper...")

    #      Generate C <-> Fortran wrapper     
    # ========================================
    fort_c_wrapper = generate_fort_c_wrapper(modules, project_name,
                                             funcs_and_args, interfaces)
    fort_wrapper_file = os.path.join(working_dir, project_name+FORT_WRAPPER_EXT)
    if not os.path.exists(fort_wrapper_file):
        with open(fort_wrapper_file,"w") as f:
            print(fort_c_wrapper, file=f)

    if verbose:
        print("FMODPY: Generating the python-compatible c wrapper...")

    #      Generate Python <-> C     
    # ===============================
    # TODO: Fix documentation of module
    # TODO: Make "generate_c_python_wrapper" automatically produce
    #       documenation for the expected usage of wrapped functions.
    c_python_wrapper = generate_c_python_wrapper(
        modules, project_name, funcs_and_args, interfaces, documentation)
    cython_file = os.path.join(working_dir, project_name+CYTHON_EXT)
    if not os.path.exists(cython_file):
        with open(cython_file, "w") as f:
            print(c_python_wrapper, file=f)


# Function for compiling and creating a python module out of the
# prepared fortran + wrapper code.
def build_mod(file_name, working_dir, mod_name, verbose=True):
    ##################################################
    # These imports are only needed for this function!
    import numpy, sysconfig
    from distutils.extension import Extension
    from distutils.core import setup
    from Cython.Build import cythonize
    ##################################################

    # Store original directory to revert back after compilation
    original_dir = os.getcwd()
    os.chdir(working_dir)

    if not os.path.exists(os.path.join(working_dir,BEFORE_DOT(file_name) + ".o")):
        if verbose: print("FMODPY: Compiling '%s'..."%(file_name))
        comp_code, stdout, stderr = run([fort_compiler,fort_compile_arg]+
                                        fort_compiler_options+[file_name])
        if comp_code != 0:
            raise(CompileError("Unexpected error in '%s'.\n"%(file_name)+
                               "\n".join(stderr)))

    # Compile the fortran wrapper 
    wrap_code = mod_name+FORT_WRAPPER_EXT
    if verbose: print("FMODPY: Compiling '%s'..."%(wrap_code))
    wrap_code, stdout, stderr = run([fort_compiler,fort_compile_arg]+
                                    fort_compiler_options+[wrap_code])
    if wrap_code != 0:
        print("\n".join(stderr))
        raise(CompileError("\nError in generated wrapper (fmodpy bug?)\n"))

    # Setup linking for creating the extension
    link_files = [f for f in os.listdir(working_dir) if 
                  (os.path.isfile(os.path.join(working_dir,f))
                   and (f[-2:] == ".o"))]
    if verbose: print("FMODPY: Linking %s..."%(link_files))
    cython_source = [mod_name+CYTHON_EXT]

    if c_compiler != None:
        # Set the compiler
        os.environ["CC"] = c_compiler

    # Get the linker options used to build python
    linker_options = sysconfig.get_config_vars().get("BLDSHARED","").split(" ")[1:]
    # Remove linker options that cause trouble
    for bad_opt in module_disallowed_linker_options:
        if bad_opt in linker_options:
            linker_options.remove(bad_opt)
    # Set the linker, with appropriate options
    os.environ["LDSHARED"] = " ".join([c_linker]+linker_options)
        
    # Generate the extension module
    ext_modules = [ Extension(
        mod_name, cython_source,
        extra_compile_args=module_compile_args,
        extra_link_args=link_files + module_link_args,
        include_dirs = [numpy.get_include()])]

    if verbose:
        print("FMODPY: Compiling extension module using, calling setup...")
        print("="*70)
        print()
    else:
        # Capture the output of setup so it doesn't bother the user
        original_stdout = sys.stdout
        original_stderr = sys.stderr
        sys.stdout = open(os.devnull, "w")
        sys.stderr = open(os.devnull, "w")

    # Manually make this file always run as "build_ext" and "--inplace"
    # Capture the output of a dry run to get the command that would be
    # used to build this module. Modify the commands appropriately.
    actual_argv = sys.argv
    sys.argv = ['', 'build_ext', '--inplace']
    dist = setup( name = mod_name, ext_modules = cythonize(ext_modules) )
    sys.argv = actual_argv

    if verbose:
        print()
        print("="*70)
        print("FMODPY: Done setting up.")
    else:
        # Reset stdout back to its normal state
        sys.stdout = original_stdout
        sys.stderr = original_stderr

    # Manipulate the path to test importing the module
    original_path = sys.path
    try:
        # First, invalidate any existing copies of the module
        try: del sys.modules[mod_name]
        except KeyError: pass
        # Second, try loading the newly created module
        sys.path = [working_dir]
        module = importlib.import_module(mod_name)
        module_path = os.path.abspath(module.__file__)
        # Finally, remove the imported module (so the user can
        # correctly import it later without any trouble)
        del sys.modules[mod_name]
    except ImportError:
        raise(LinkError("\nUnable to successfully import module.\n Perhaps the "+
                        "relevant fortran modules were not available?"))
    finally:
        # Rever the system path no matter what
        sys.path = original_path
    if verbose:
        print("FMODPY: Successfully built and imported module.")

    # Revert to the original directory
    os.chdir(original_dir)

    return module_path

# Function for automating the entire fortran<->python wrapping
#   process. This is the top-level function built for standard usage.
#   This function automatically checks the modification times of the
#   output module as well as the source fortran in order to only
#   re-compile when necessary. Automatically wraps fortran, copies over
#   all files in the source directory as potential dependencies. 
# 
#  INPUTS:
#    input_fortran_file -- str, relative or absolute path to fortran source.
# 
#  OPTIONAL:
#    mod_name           -- str, name of output python module. Defaults
#                          to be BEFORE_DOT(os.path.basename(input_fortran_file))
#    requested_funcs    -- list<str>, names of functions that should
#                          be wrapped from fortran.
#    force_rebuild      -- True if you want to rebuild even though the
#                          input_fortran_file has not been modified
#                          since the last compilation of the module.
#    working_direcotry  -- The directory to store the wrapper code for
#                          input_fortran_file. If provided, it is
#                          assumed that the directory should not be
#                          removed automatically.
#    output_direcotry   -- The directory to store the output python
#                          module. Defaults to os.getcwd()
#    verbose            -- True if you want updates and printouts
#                          about the wrapping process.
# 
# In order to take finer control over the python-module construction
# process, modify the following globals stored in fmodpy:
# 
#    fort_compiler         -- str, should be accessible from a system call.
#    fort_compile_arg      -- str, defaults to '-c'.
#    fort_compiler_options -- list<str>, extra options for fortran compilation.
#    c_linker   -- str, Updates 'LDSHARED' envrionment variable to
#                  indirectly control c-linker used by distutils.
#    c_compiler -- str, Updates 'CC' environment variable to
#                  indirectly control c-compiler used by distutils.
#    module_compile_args -- list<str> 'extra_compile_args' argument
#                           passed into Extension constructor for distutils.
#    module_link_args    -- list<str> 'extra_link_args' argument
#                           passed into Extension constructor for
#                           distutils. Needs to allow chosen
#                           c-compiler to link to fortran. 
#    module_disallowed_linker_options -- list<str> of arguments to the
#                                        c-linker that may be causing
#                                        linkage failure. This happens
#                                        sometimes when the system default
#                                        compiler is not the same as python's.
# 
def wrap(input_fortran_file, mod_name="", requested_funcs=[],
         force_rebuild=False, working_directory="",
         output_directory="", verbose=False):
    # Set the default output directory
    if len(output_directory) == 0:
        output_directory = os.getcwd()

    source_file = os.path.basename(input_fortran_file)
    source_dir = os.path.dirname(os.path.abspath(input_fortran_file))
    if len(mod_name) == 0:
        mod_name = BEFORE_DOT(source_file)
        if not LEGAL_MODULE_NAME(mod_name):
            raise(NameError(("'%s' is not an allowed module name,\n"+
                             " must match the regexp `^[a-zA-z_]+"+
                             "[a-zA-Z0-9_]*`. Set the name with:\n"+
                             " fmodpy.wrap(<file>, mod_name=<legal name>)"+
                             " OR\n $ fmodpy <file> mod_name=<legal_name>")%(mod_name)))

        working_dir_name = FMODPY_DIR_PREFIX+mod_name
        working_dir = os.path.join(source_dir, working_dir_name)

    # Get the last modification time of the module (if it exists already)
    # Make sure that the output directory is in the sys path so that
    # the time-check import will work correctly.
    sys.path = [output_directory] + sys.path
    try:
        mod = importlib.import_module(mod_name)
        module_mod_time = os.path.getmtime(mod.__file__)
    except ImportError:
        mod = None
        module_mod_time = 0
    except:
        # TODO: Existing module was corrupt, should we warn the user or ignore?
        module_mod_time = 0
    # Reset the sys path
    sys.path = sys.path[1:]

    # Exit if the module has been built since the last update of the
    # source file and the user does *not* want to force a reconstruction.
    source_file_mod_time = os.path.getmtime(input_fortran_file)
    if (not force_rebuild) and (source_file_mod_time < module_mod_time):
        if verbose:
            print("FMODPY: No new modifications to '%s' module, exiting."%(mod_name))
        return

    # Prepare (create) the working directory, copy in necessary files
    working_dir = prepare_working_directory(
        source_file, source_dir, mod_name, working_directory, verbose)

    # Generate the wrappers for going from python <-> fortran
    fortran_to_python(input_fortran_file, working_dir, mod_name,
                      requested_funcs, verbose)

    # Build the python module by compiling all components
    mod_path = build_mod(source_file, working_dir, mod_name, verbose=verbose)

    # Copy out the module from the working_dir to the output directory
    if (output_directory != working_dir):
        # Create the output directory if necessary
        if not os.path.exists(output_directory):
            os.makedirs(output_directory, exists_ok=True)
        # Copy the module into the output directory
        shutil.copyfile(mod_path, os.path.join(
            output_directory,os.path.basename(mod_path)))
        if verbose:
            print("FMODPY: Copied python module to '%s'."%(output_directory))

    if len(working_directory) == 0:
        # Remove the automatically created working directory
        shutil.rmtree(working_dir)
        if verbose:
            print("FMODPY: Removed working directory.")
            print()

# These must be the last declared globals for them to include
# everything, allows for automatically parsing command line arguments
# the same way as the user would set the variables in practice.
USER_GLOBAL_CASTS = {
    type(None) : lambda s: s,
    str        : lambda s: s,
    list       : lambda s: s[1:-1].split(","),
    bool       : lambda s: bool(s.lower().strip("false").replace("0","")),
}
USER_MODIFIABLE_GLOBALS = [n for n,v in globals().items() if
                           (n.lower() == n) and (n[:2] != "__") and
                           (type(v) in USER_GLOBAL_CASTS)]

# Making this module accessible by being called directly from command line.
if __name__ == "__main__":
    import traceback, inspect 

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
    # Generate a list of modifiable "wrap" parameters
    wrap_parameters = inspect.signature(wrap).parameters
    wrap_parameters = [p for p in wrap_parameters if (not wrap_parameters[p].default)]
    wrap_kwargs = {p:inspect.signature(wrap).parameters[p].default
                   for p in wrap_parameters}

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
        wrap_kwargs["requested_funcs"] = wrap_kwargs["requested_funcs"] + extra_args
        print("requested_funcs = %s -> %s"%([], wrap_kwargs["requested_funcs"]))

    # Call "wrap"
    wrap(file_path, **wrap_kwargs)


