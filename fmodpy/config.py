import os, sysconfig
import numpy as NP

# Default configurable variables.
omp                = False
blas               = False
lapack             = False
verbose            = False
verbose_module     = True
autocompile        = True
wrap               = True
rebuild            = False
show_warnings      = True
debug_line_numbers = False
implicit_typing    = False
end_is_named       = True
delete_destination = False # hard-delete destination directory if it already exists
log_file           = os.devnull
f_compiler         = 'gfortran'
f_compiler_args    = '-fPIC -shared -O3'
link_omp           = '-fopenmp'
link_blas          = '-lblas'
link_lapack        = '-lblas -llapack'
home_directory     = os.path.expanduser("~")
libraries          = "/usr/lib " + " ".join(NP.__path__)
library_recursion  = 2
library_extensions = "so dylib"
symbol_command     = 'nm -gU "{path}" 2> /dev/null || nm -gD "{path}" 2> /dev/null'
config_file        = ".fmodpy.py"
wait_warning_sec   = 5 # number of seconds to wait before warning about automatic compilation

# --------------------------------------------------------------------
#      Development globals, not intended to be changed by users.
# 
# All of these variables should have expected types.
BOOL_CONFIG_VARS = ['omp', 'blas', 'lapack', 'verbose', 'autocompile',
                    'wrap', 'rebuild', 'show_warnings',
                    'debug_line_numbers', 'implicit_typing',
                    'end_is_named', 'delete_destination']
LIST_CONFIG_VARS = ['f_compiler_args', 'link_omp', 'link_blas',
                    'link_lapack', 'libraries', 'library_extensions']
# File related maniplation arguments
PY_EXT = ".py"
FORT_EXT = ".f90"
PYTHON_WRAPPER_EXT = "_python_wrapper"
FORT_WRAPPER_EXT = "_c_wrapper"+FORT_EXT
GET_SIZE_PROG_FILE = "fmodpy_get_size"+FORT_EXT
GET_SIZE_EXEC_FILE = "fmodpy_get_size"
# --------------------------------------------------------------------

# Automatically handle printing for status updates.
#   WARNING: Do not use this function for warnings. Use `warnings.warn`.
#   WARNING: Do not use this function for errors. Use `raise(...)`.
# 
# Custom print function (allows for line numbers to be automatically
# added to all print statements, easily controls verbosity level).
def fmodpy_print(*args, **kwargs):
    # Skip all print statements if verbosity is off.
    global verbose
    if (not verbose): return
    # Set the log file.
    global log_file
    if (log_file == os.devnull): 
        import sys
        log_file = sys.stdout
    # Add information about where the bit is printed from, if turned on.
    global debug_line_numbers    
    if debug_line_numbers:
        import inspect
        # Add the calling file and line to the print statement
        calling_function = inspect.stack()[1]
        calling_file = calling_function.filename
        calling_file = os.path.basename(calling_file)
        calling_line = calling_function.lineno
        args += (f'({calling_file} line {calling_line})',) 
    # For all print operations, force a "flush" output.
    kwargs["file"] = log_file
    kwargs["flush"] = True
    print(*args, **kwargs)


# Execute a blocking command with a subprocess, on completion provide
# the return code, stdout as string, and stderr as string. This should
# work across all Python3.x as well as cross-platform.
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
    import sys, subprocess
    # For Python 3.x and x < 6, the encoding is a string by default
    # For Python 3.6 and later the encoding can be given as an arguemnt
    if sys.version_info >= (3,6):
        popen_kwargs.update( dict(encoding="UTF-8") )
    proc = subprocess.Popen(command, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, **popen_kwargs)
    stdout, stderr = proc.communicate()
    # Before python 3.6, the encoding had to be handled after collection
    if (sys.version_info < (3,6)):
        if (type(stdout) != str): stdout = str(stdout, encoding="UTF-8")
        if (type(stderr) != str): stderr = str(stderr, encoding="UTF-8")
    # Remove Windows specific characters and split by the new line.
    if stdout: stdout = stdout.replace("\r","").split("\n")
    else:      stdout = ""
    if stderr: stderr = stderr.replace("\r","").split("\n")
    else:      stderr = ""
    # Return the exit code, standard out, and standard error.
    return proc.returncode, stdout, stderr


# Configure the current 'fmodpy' runtime. Start with default values in
# this file, load any global defaults over those, and finally override
# again with provided keyword arguments.
def load_config(**kwargs):
    # Get the "globals" for this file, this is the configuration of fmodpy.
    fmodpy_config = globals()

    # Identify those elements of "fmodpy.config" that should not be
    # set or considered when printing out configuration.
    modules = {"os", "sysconfig"}
    func_type = type(lambda:None)
    functions = {k for k in fmodpy_config if
                 type(fmodpy_config[k]) == func_type}
    # Set the default configuration as the current configuration.
    config = { k:fmodpy_config[k] for k in fmodpy_config
               if (k[0].isalpha() and k.islower()) and
               (k not in modules) and (k not in functions) }

    # Check to make sure that all variables given specify only those
    # that are allowed to be specified.
    for var in kwargs:
        if (var not in config):
            from fmodpy.exceptions import UnrecognizedConfiguration
            raise(UnrecognizedConfiguration(f"Configuration for '{var}' is given, but that variable does not exist."))
        if (var[0].isupper() or (not var[0].isalpha())):
            from fmodpy.exceptions import IllegalConfiguration
            raise(IllegalConfiguration(f"The variable '{var}' is not allowed to be configured."))

    # Update the 'config' dictionary with the provided keyword arguments.
    config.update( kwargs )

    # Make sure the path names do not have spaces.
    if any(' ' in str(config[k]) for k in ('f_compiler',)):
        from fmodpy.exceptions import NotAllowedPath
        raise(NotAllowedPath("Spaces cannot be included in compiler or linker paths."))

    # Convert list-type configurations into lists (from strings).
    for var in LIST_CONFIG_VARS:
        if (type(config[var]) is str):
            config[var] = config[var].split()
        elif (type(config[var]) is not list):
            from fmodpy.exceptions import IllegalConfiguration
            raise(IllegalConfiguration(f"The variable '{var}' is supposed to be a list or string, but is neither."))

    # Make sure that boolean-type configurations are booleans (from strings).
    for var in BOOL_CONFIG_VARS:
        if (type(config[var]) is str):
            config[var] = (config[var].lower().strip() == 'true')
        elif (type(config[var]) is not bool):
            from fmodpy.exceptions import IllegalConfiguration
            raise(IllegalConfiguration(f"The variable '{var}' is supposed to be a bool or string, but is neither."))

    # If 'lblas' is True, then add BLAS compilation and
    # link arguments to the list of arguments already.
    if config["blas"]:
        for l in config["link_blas"]:
            if (l not in config["f_compiler_args"]):
                config["f_compiler_args"] += [l]

    # If 'llapack' is True, then add LAPACK compilation and
    # link arguments to the list of arguments already.
    if config["lapack"]:
        for l in config["link_lapack"]:
            if (l not in config["f_compiler_args"]):
                config["f_compiler_args"] += [l]

    # If 'omp' is True, then add OpenMP compilation and link arguments
    # to the list of arguments already.
    if config["omp"]:
        for l in config["link_omp"]:
            if (l not in config["f_compiler_args"]):
                config["f_compiler_args"] += [l]

    # Set all of the configuration variables as module-wide globals.
    for var in config: fmodpy_config[var] = config[var]

    # Return the current configuration.
    return config


# --------------------------------------------------------------------
# --------------------------------------------------------------------


# Read a configuration file into a Python dictionary.
def read_config_file(path):
    with open(path) as f:
        # Get all lines that have one "=" split and stripped into nice strings.
        # Strip quote characters from edges of strings (when user assigned a string).
        lines = ((' '.join(var.strip().strip('\'"').split())  for var in l.strip().split('='))
                 for l in f.readlines() if (l.count('=') == 1))
    return dict(lines)

# Load local settings if they are available.
if os.path.exists(os.path.join(home_directory,config_file)):
    global_config = read_config_file(
        os.path.join(home_directory,config_file))
else: global_config = {}
# Make sure all provided settings are recognized (and allowable).
for var in global_config:
    if (var.strip()[:1] == "#"): continue # Skip commented lines.
    if var not in globals():
        from fmodpy.exceptions import UnrecognizedConfiguration
        raise(UnrecognizedConfiguration(f"Configuration for '{var}' is given in config file, but that variable does not exist."))
# Update the configuration of fmodpy.
globals().update(global_config)
# Delete the local variables (so they don't hang around).
if (len(global_config) > 0): del var
del read_config_file
del global_config
# Load in the global configuration file.
load_config()
