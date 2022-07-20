# --------------------------------------------------------------------
#                              fmodpy
# 
#       an automatic fortran wrapper (and importer) for python.
# 
# This program is designed to processes a source fortran file and wrap
# all contained code into a python module.
# 
#   fimport -- Given a path to a Fortran source file,
#              wrap it and return a Python module.
# 
# For configuration and future usage of `fmodpy` on a single machine,
# use the following function.
# 
#   configure -- Three behaviors related to `fmodpy` configuration:
#                  no arguments -> Print current configuration and return.
#                  str, str, ... -> Remove these configurations from global config file.
#                  key=value, ... -> Assign these configurations in global config file.
# 
# --------------------------------------------------------------------

# Overwrite the "print" function to add "FMODPY" specific printouts.
from fmodpy.config import fmodpy_print as print
from fmodpy.config import libraries

# Function for automating the entire fortran<->python wrapping
#   process. This is the top-level function built for standard usage.
#   This function automatically checks the modification times of the
#   output module as well as the source fortran in order to only
#   re-compile when necessary. Automatically wraps fortran, makes
#   symbolic links to all files in the source directory as potential
#   dependencies for building, generates a wrapper in Fortran, 
#   generates a wrapper using Python ctypes module, then defines a 
#   Python module that compiles the wrappers and Fortran source,
#   providing a Python interface to the Fortran code.
# 
#  INPUTS:
#    input_fortran_file -- str, relative or absolute path to fortran source.
# 
#  OPTIONAL:
#    name          -- str, name of output python module. Defaults to be
#                     `before_dot(os.path.basename(input_fortran_file))`
#    build_dir     -- str, the directory to store the wrapper code for
#                     `input_fortran_file`. If provided, it is
#                     assumed that the directory should not be
#                     removed automatically, otherwise a temporary
#                     directory is created, used, and deleted.
#    output_dir    -- str, the directory to store the output python
#                     module. Defaults to `os.getcwd()`.
#    dependencies  -- list of str, paths to Fortran files that should be
#                     checked for changes when compiling the final module.
#    symbols       -- list of str, names of symbols that are expected to be
#                     defined by local libraries. list of tuple of str, pairs 
#                     of symbol names and an expected substring in the library
#                     file that contains that symbol's definition.
#    **kwargs      -- Any configuration options relevant to this
#                     compilation passed as keyword arguments, 
#                     see `help(fmodpy)` for more information, or
#                     run again with 'verbose=True' to see options.
# 
#   KEYWORD OPTIONS (some important ones, more in 'fmodpy.configure'):
#    autocompile -- bool, whether or not automatic compilation of
#                   dependancies should be attempted.
#    rebuild     -- bool, True if you want to rebuild even though the
#                   `input_fortran_file` has not been modified
#                   since the last compilation of the module.
#    wrap        -- bool, True if the wrapper code should be generated
#                   even if wrapper code already exists in `build_dir`,
#                   use False when manually modifying generated wrappers.
#    show_warnings -- bool, if miscellaneous warnings should be printed.
# 
def fimport(input_fortran_file, name=None, build_dir=None,
            output_dir=None, dependencies=None, symbols=None, **kwargs):
    # Import parsing functions.
    from fmodpy.parsing import after_dot, before_dot, legal_module_name
    from fmodpy.config import run, load_config, \
        FORT_WRAPPER_EXT, PYTHON_WRAPPER_EXT, PY_EXT
    # Import "os" for os operations and "importlib" for importing a module.
    import os, sys, shutil, importlib

    # Configure this runtime of fmodpy 
    pre_config = load_config() # <- gets the configuration dictionary
    if (len(kwargs) > 0): load_config(**kwargs) # <- assigns given configuration
    # Import some locally used settings.
    from fmodpy.config import wrap, rebuild, autocompile, \
        verbose_module, f_compiler, f_compiler_args, delete_destination

    # Print the configuration (when verbose).
    print()
    print("_"*70)
    print("fimport")
    print()
    print("fmodpy configuration:")
    c = load_config()
    for n in sorted(c): print(f"  {n} = {str([c[n]])[1:-1]}")
    if (len(c) > 0): del n
    del c

    # Get the source file and the source directory.
    source_path = os.path.abspath(input_fortran_file)
    source_file = os.path.basename(source_path)
    source_dir = os.path.dirname(source_path)
    

    # If the name is not defined, then try to automatically produce it.
    if (name is None): name = before_dot(source_file)

    # Check to make sure that the module name is legal.
    if not legal_module_name(name):
        from fmodpy.exceptions import NameError
        raise(NameError((f"'{name}' is not an allowed module name,\n"+
                         " must match the regexp `^[a-zA-Z_]+"+
                         "[a-zA-Z0-9_]*`. Set the name with:\n"+
                         " fmodpy.fimport(<file>, name='<legal name>')"+
                         " OR\n $ fmodpy <file> name=<legal_name>")))

    # Set the default output directory
    if (output_dir is None): output_dir = os.getcwd()
    else:                    output_dir = os.path.abspath(output_dir)

    # Initialize the list of depended files if they were not given.
    if (dependencies is None): dependencies = [source_file]
    # Given a string, assume it should be split by spaces.
    elif (type(dependencies) is str):  dependencies = dependencies.split()
    # If something else was given, then copy it into a list (because files will be appended).
    else: dependencies = list(dependencies)
    # Make sure that the source file is in the list of dependencies.
    if (source_file not in dependencies):
        dependencies.append(source_file)

    # Determine whether or not the module needs to be rebuilt.
    should_rebuild = rebuild or should_rebuild_module(
        [os.path.join(source_dir, d) for d in dependencies], name, output_dir)
    if not should_rebuild:
        print()
        print("No new modifications to '%s' module, exiting."%(name))
        print("^"*70)
        return importlib.import_module(name)

    # Generate the names of files that will be created by this
    # program, so that we can copy them to an "old" directory if necessary.
    fortran_wrapper_file = name+FORT_WRAPPER_EXT
    python_wrapper_file = name+PYTHON_WRAPPER_EXT+PY_EXT
    build_dir_name = build_dir if (build_dir is not None) else "temporary directory"
    bar_width = 23 + max(map(len,(source_dir, source_file, name, build_dir_name,
                                  fortran_wrapper_file, python_wrapper_file, output_dir)))
    print()
    print("="*bar_width)
    print("Input file directory: ",source_dir)
    print("Input file name:      ",source_file)
    print("Base module name:     ",name)
    print("Using build dir:      ",build_dir_name)
    print("  fortran wrapper:    ",fortran_wrapper_file)
    print("  python wrapper:     ",python_wrapper_file)
    print("Output module path:   ",output_dir)
    print("="*bar_width)
    print()
 
    # Prepare the build directory, link in the necessary files.
    build_dir, temp_dir = prepare_build_directory(source_dir, build_dir)

    # Find any required sources and link them into the build directory.
    if (symbols is not None):
        symbol_files = []
        print(f"Searching libraries for symbols:\n  {symbols}")
        for symbol in symbols:
            if (type(symbol) is not str):
                symbol, filename_includes = symbol
                symbol_kwargs = dict(filename_includes=filename_includes)
            else: symbol_kwargs = {}
            lib_file = load_symbol(symbol, **symbol_kwargs)
            lib_name = os.path.basename(lib_file)
            lib_dest = os.path.join(build_dir, lib_name)
            if (not os.path.exists(lib_dest)):
                print(f" sym-linking '{lib_file}' to '{lib_dest}'")
                os.symlink(os.path.realpath(lib_file), lib_dest)
            else:
                print(f" skpping sym-link to '{lib_file}', already exists at '{lib_dest}'")
            symbol_files.append(lib_name)
        print()
        dependencies = symbol_files + dependencies
    else:
        symbol_files = []

    # Automatically compile fortran files.
    if autocompile:
        print("Attempting to autocompile..")
        built, failed = autocompile_files(build_dir, dependencies)
        dependencies = dependencies + [f for f in built if (f not in dependencies)]
        print()
        if (len(built) > 0):
            print("Identified dependencies:")
            for f in built: print(" ",os.path.basename(f))
            print()

    # Write the wrappers to the files so they can be compiled.
    existing_fortran_wrapper_path = os.path.join(output_dir, name, fortran_wrapper_file)
    fortran_wrapper_path = os.path.join(build_dir, fortran_wrapper_file)
    existing_python_wrapper_path = os.path.join(output_dir, name, python_wrapper_file)
    python_wrapper_path = os.path.join(build_dir, python_wrapper_file)
    # Move the existing wrapper to the new build directory (if it is to be kept).
    if (os.path.exists(existing_fortran_wrapper_path) and (not wrap)):
        shutil.move(existing_fortran_wrapper_path, fortran_wrapper_path)
    if (os.path.exists(existing_python_wrapper_path) and (not wrap)):
        shutil.move(existing_python_wrapper_path, python_wrapper_path)
    # Check for the existence of the wrapper files (in the build or output directories).
    fortran_wrapper_exists = os.path.exists(fortran_wrapper_path)
    python_wrapper_exists = os.path.exists(python_wrapper_path)
    # Generate the wrappers for going from python <-> fortran.
    if (wrap or (not fortran_wrapper_exists) or (not python_wrapper_exists)):
        fortran_wrapper, python_wrapper = make_wrapper(
            source_path, build_dir, name)
        # Add fortran wrapper to dependencies and remove any
        #  duplicates from the "dependencies" list.
        dependencies.append( os.path.basename(fortran_wrapper_path) )
        i = len(dependencies)
        while (i > 1):
            i -= 1
            while (dependencies.index(dependencies[i]) < i):
                dependencies.pop(dependencies.index(dependencies[i]))
                i -= 1
        # Fill all the missing components of the python_wrapper string.
        python_wrapper = python_wrapper.format(
            verbose_module = verbose_module,
            f_compiler = f_compiler,
            shared_object_name = name,
            f_compiler_args = str(f_compiler_args),
            dependencies = dependencies,
            symbol_files = symbol_files,
        )
    # Write the wrapper files if this program is supposed to.
    if (not fortran_wrapper_exists) or wrap:
        with open(fortran_wrapper_path, "w") as f: f.write( fortran_wrapper )
    if (not python_wrapper_exists) or wrap:
        with open(python_wrapper_path, "w") as f: f.write( python_wrapper )

    # Make the `__init__.py` for the newly created Python module a link.
    init_file = os.path.join(build_dir,"__init__.py")
    if os.path.exists(init_file): os.remove(init_file)
    print()
    print(f"Making symlink from '{python_wrapper_file}' to '__init__.py'")
    print()
    os.symlink(os.path.join(".", python_wrapper_file), init_file)
    # Generate the final module path, move into location.
    final_module_path = os.path.join(output_dir, name)
    # Move the compiled module to the output directory.
    print("Moving from:", f"  {build_dir}", "to", f"  {final_module_path}", sep="\n")
    if not (build_dir == final_module_path):
        # Remove the existing wrapper module if it exists.
        if os.path.exists(final_module_path):
            if (delete_destination):
                print(f" deleting existing directory at '{final_module_path}'..")
                shutil.rmtree(final_module_path)
            else:
                # Keep previous compilation in "old" directory.
                old_module_path = final_module_path + "_OLD"
                # Remove old directories permanently.
                if os.path.exists(old_module_path):
                    print(f" removing old wrapper of '{name}' at '{old_module_path}'..")
                    shutil.rmtree(old_module_path)
                print(f" moving existing directory at '{final_module_path}' to '{old_module_path}'..")
                shutil.move(final_module_path, old_module_path)
        # Move the compiled wrapper to the destination.
        shutil.move(build_dir, final_module_path)
        # Remove all files (symlinks) that are not dependencies.
        all_kept_files = set(dependencies) | {fortran_wrapper_file, python_wrapper_file, "__init__.py"}
        for p in os.listdir(final_module_path):
            if ((p not in all_kept_files) and (after_dot(p) != 'mod')):
                print(f" removing unnecessary file '{p}'..")
                p = os.path.join(final_module_path, p)
                os.remove(p)

    print(f"\nFinished making module '{name}'.\n")
    print("^"*70)
    print()

    # Clean up the the temporary directory if one was created.
    if temp_dir is not None:
        try: temp_dir.cleanup()
        except FileNotFoundError: pass
        del temp_dir

    # Re-configure 'fmodpy' to work the way it did before this execution.
    if (len(kwargs) > 0): load_config(**pre_config)

    # (Re)Import the module.
    sys.path.insert(0, output_dir)
    module = importlib.import_module(name)
    module = importlib.reload(module)
    sys.path.pop(0)
    # Return the module to be stored as a variable.
    return module

# ====================================================================

# Given a file name to a source python file and a build directory,
# make a python wrapper that wraps the given fortran file as if it were
# a python module. Allow for the import of all modules (with functions),
# and plain funcitons / subroutines listed in the fortran file.
def make_wrapper(source_file, build_dir, module_name):
    import os
    from fmodpy.parsing import simplify_fortran_file, after_dot
    from fmodpy.parsing.file import Fortran
    from fmodpy.exceptions import ParseError

    # Make a simplified version of the fortran file that only contains
    # the relevant syntax to defining a wrapper in python.
    is_fixed_format = after_dot(source_file) == "f"
    simplified_file = simplify_fortran_file(source_file, is_fixed_format)
    # Write the simplified file to the build directory.
    simplified_file_path = os.path.join(build_dir, "fmodpy_simplified_"+os.path.basename(source_file))
    with open(simplified_file_path, "w") as f: f.write("\n".join(simplified_file))
    # Parse the simplified fortran file into an abstract syntax.
    try:
        abstraction = Fortran(simplified_file)
    except ParseError as exc:
        from fmodpy.config import end_is_named
        if (is_fixed_format and (not end_is_named)):
            print("\nWARNING: Encountered unnamed 'END' in source for a fixed format file. Consider setting 'end_is_named=False' configuration when wrapping this file.\n")
        raise(exc)
    print("-"*70)
    print(abstraction)
    print("-"*70)
    # Generate the C <-> Fortran code.
    fortran_file = abstraction.generate_fortran()
    # Evaluate the sizes of all the Fortran variables.
    abstraction.eval_sizes(build_dir)
    # Generate the python <-> C code.
    python_file = abstraction.generate_python()
    # Return the two files that can be used to construct the wrapper.
    return fortran_file, python_file

# ====================================================================

# Given a build directory (containing some fortran files), repeatedly
# attempt to compile all files in the build directory until there are
# no successfully compiled files.
def autocompile_files(build_dir, dependencies):
    import os, time
    from fmodpy.parsing import after_dot
    # Get configuration parameters.
    from fmodpy.config import run, f_compiler, f_compiler_args, \
        wait_warning_sec, GET_SIZE_PROG_FILE, GET_SIZE_EXEC_FILE
    # Make compilation option substititions for speed.
    f_compiler_args = [arg if (arg not in {"-O3", "-O2", "-O1"}) else "-O0"
                       for arg in f_compiler_args]
    # Set the wait time and start time.
    wait_warning_sec = int(wait_warning_sec)
    start_time = time.time()
    # Try and compile the rest of the files (that might be fortran) in
    # the working directory in case any are needed for linking.
    should_compile = dependencies[:]
    dependencies = set(dependencies)
    print()
    # generate the list of files that we sould try to autocompile
    for f in os.listdir(build_dir):
        f = os.path.join(build_dir, f.strip())
        # Skip the preprocessed file, the size program, and directories
        if ( (os.path.isdir(f)) or
             ("." not in f) or
             ("f" not in after_dot(f)) or
             (GET_SIZE_PROG_FILE in f) ):
            print(f" skipping '{f}'")
            continue
        # Try opening the file, if it can't be decoded, then skip
        # Make sure the file does not have any immediate exclusions,
        # if it does then skip it
        try:
            with open(f) as fort_file:
                f = os.path.basename(f)
                print(f" reading '{f}' to check if it can be autocompiled.. ", end="")
                exclude_this_file = False
                # Read through the file, look for exclusions
                for line in fort_file.readlines():
                    line = line.strip().upper().split()
                    if (len(line) > 0) and ("PROGRAM" in line[0]):
                        exclude_this_file = True
                        break
                if exclude_this_file:
                    print(f"no. The file '{f}' contains 'PROGRAM'.")
                    continue
                else: print("yes.")
        # Some failure occurred while reading that file, skip it
        except UnicodeDecodeError: continue
        # No failures or obvious red-flags, this file might be useful
        should_compile.append(f)
    # Handle dependencies by doing rounds of compilation, presuming
    # only files with fewest dependencies will compile first
    ordered_depends = []
    successes = {None}
    made_mod = set()
    failed = set()
    # Get the list of existing "mod" files (Fortran module definitions).
    current_mod_files = sum((1 for f in os.listdir(build_dir) if after_dot(f) == "mod"))
    previous_mod_files = current_mod_files
    # Continue rounds until (everything compiled) or (no successes nor new mod files)
    print()
    while (len(should_compile) > 0) and (
            (len(successes) > 0) or (len(made_mod) > 0)):
        successes = set()
        made_mod = set()
        for f in should_compile:
            # Check to see if compilation is taking unexpectedly long.
            if (time.time()-start_time > wait_warning_sec):
                import warnings
                warnings.warn("\n  Automatic compilation is taking longer than expected, consider"+
                              "\n  providing explicitly ordered dependencies (precompiled or not) with"+
                              "\n    fmodpy.fimport('<target>', dependencies=['<path>', ...], ...)\n")
                start_time = float('inf') # <- do this to prevent further warnings

            # Try to compile all files that have "f" in the extension.
            print(f"Compiling '{f}'.. ")
            # Reuse the "GET_SIZE_EXEC_FILE" name for compilation checks.
            compiled_file_path = os.path.join(build_dir, GET_SIZE_EXEC_FILE)
            cmd = [f_compiler] + f_compiler_args + ["-o", compiled_file_path] + \
                  [d for d in ordered_depends if (d != f)] + [f]
            print(f" {' '.join(cmd)}".replace(build_dir,"."))
            code, stdout, stderr = run(cmd, cwd=build_dir)
            # Update the list of existing mod files.
            previous_mod_files = current_mod_files
            current_mod_files = sum((1 for f in os.listdir(build_dir) if after_dot(f) == "mod"))
            if (code == 0):
                successes.add(f)
                if (f not in ordered_depends):
                    ordered_depends.append(f)
                print("  success.")
                # If all dependencies have been successfully
                #  compiled, then exit (because we are done).
                if (f in dependencies):
                    dependencies.remove(f)
                if (len(dependencies) == 0):
                    should_compile = []
                # Since a file was successfully compiled, break, which
                #  will trigger another attempt at compiling the
                #  target file (if there is a target).
                break
            elif (current_mod_files > previous_mod_files):
                print("  failed, but created new '.mod' file, continuing.")
                made_mod.add(f)
                if (f not in ordered_depends):
                    ordered_depends.append(f)
            else:
                # Record failed compilations.
                if (f not in failed): failed.add(f)
                print("  failed.")
                print("NAME:", f)
                
                if (max(len(stdout), len(stderr)) > 0): print('-'*70)
                if len(stdout) > 0:
                    print("STANDARD OUTPUT:")
                    print("\n".join(stdout))
                if len(stderr) > 0:
                    stderr_str = "\n".join(stderr)
                    print("STANDARD ERROR:")
                    print(stderr_str)
                    # If there is a pure "error" in the target file, raise it.
                    if ("\nError:" in stderr_str) and (f in dependencies):
                        from fmodpy.exceptions import CompileError
                        raise CompileError(f"Failed to compile '{f}' with error:\n\n{stderr_str}")
                if (max(len(stdout), len(stderr)) > 0): print('-'*70)
        # Remove the files that were successfully compiled from
        # the list of "should_compile" and the list of "failed".
        for f in successes:
            if (f in failed):
                failed.remove(f)
            # The only way that "f" could not be in "should_compile"
            #  is if all dependencies were compiled successfully and
            #  "should_compile" was overwritten with an empty list.
            if (f not in made_mod) and (len(should_compile) > 0):
                should_compile.remove(f)
    # Log the files that failed to compile.
    for f in should_compile: print(f"Failed to compile '{f}'.")
    # Raise an error if there was a target file and it was not compiled.
    failed_dependencies = dependencies & failed
    if (len(failed_dependencies) > 0):
        from fmodpy.exceptions import CompileError
        raise(CompileError(f"Failed to compile {failed_dependencies}.\n  Current compilation arguments are:\n    {f_compiler_args}\n  Is a necessary compilation argument or dependency missing?"))
    # Return the list of files that were successfully compiled in
    # the order they were compiled and the files that failed to compile.
    return ordered_depends, failed

# ====================================================================

# Given a symbol name, try to locate the library that defines it
#   and load the symbol as a CDLL so that it is accessible.
def load_symbol(symbol, filename_includes=""):
    import os, subprocess, re, ctypes
    from fmodpy.config import libraries, library_recursion, library_extensions, symbol_command
    # This code will find the first match for the symbol in a library.
    max_recursion = library_recursion
    versions = set()
    directories = [(p,max_recursion) for p in libraries]
    checked_files = 0
    symbol_file = None
    while ((len(directories) > 0) and (symbol_file is None)):
        directory, max_recursion = directories.pop(0)
        # Skip nonexistant directories.
        if (not os.path.exists(directory)):
            print(f" MISSING - no such path '{directory}'.")
            continue
        if (not os.access(directory, os.R_OK)):
            print(f" SKIPPING - not allowed to read '{directory}'.")
            continue
        # Get all files with the desired extension.
        file_paths = [
            os.path.join(directory, f)
            for f in os.listdir(directory)
            if  (not os.path.isdir(os.path.join(directory, f)))
            and (any(ext in library_extensions for ext in f.split(".")[1:]))
            and (filename_includes in f)
        ]
        # Iterate over candidate files and check their symbol tables.
        for f in file_paths:
            checked_files += 1
            print(f" checking for '{symbol}' in '{f}' with '{symbol_command.format(path=f)}'.")
            symbols = str(subprocess.run(symbol_command.format(path=f), shell=True,
                                         capture_output=True).stdout, "utf8")
            if (symbol+"\n" in symbols):
                symbol_file = f
                break
        # Recurse into directories (up to provided max depth).
        if ((max_recursion > 0) and (symbol_file is None)):
            directories += [
                (os.path.join(directory, d), max_recursion-1)
                for d in os.listdir(directory)
                if os.path.isdir(os.path.join(directory, d))
            ]
    # Raise an error if no symbol file was found.
    if (symbol_file is None):
        raise(NotImplementedError(f"After checking {checked_files} files, the symbol '{symbol}' could not be found in any of the fmodpy.config.libraries paths:\n   {libraries}"))
    # Load the library globally (so it is accessible to later codes) and return its path.
    ctypes.CDLL(symbol_file, mode=ctypes.RTLD_GLOBAL)
    return symbol_file

# ====================================================================

# Given the path to the file that we are creating an extension for,
# create and prepare a build directory for the project compilation
def prepare_build_directory(source_dir, build_dir):
    import os
    # Create a build directory.
    if (build_dir is None):
        # Create a temporary directory for building.
        from tempfile import TemporaryDirectory
        temp_dir = TemporaryDirectory()
        build_dir = temp_dir.name
        print(f"Using temporary build directory at '{build_dir}'.")
    else:
        # Otherwise, assume a path was given, convert to absolute form.
        temp_dir = None
        build_dir = os.path.abspath(build_dir)
        # Create the directory for the build if it does not exist.
        if (not os.path.exists(build_dir)):
            print(f"Making build directory at '{build_dir}'.")
            os.makedirs(build_dir) #, exist_ok=True)

    # If the working directory is not the same as the file directory,
    # copy all the contents of the file directory into the working
    # directory (in case any of them are used by the fortran project)
    if (os.path.abspath(source_dir) != os.path.abspath(build_dir)):
        print("Build directory is different from source directory..")
        for f in os.listdir(source_dir):
            # Get the full path of this source file.
            source = os.path.join(source_dir,f)
            # Do not make links to python files (because they shouldn't be needed)
            if (f[-3:] == ".py"): continue
            # Do not make links to the directory itself.
            if (source == build_dir): continue
            # Create a symbolic link to all source files in the build directory.
            destination = os.path.join(build_dir,f)
            print(" sym-linking '%s' to '%s'"%(source, destination))
            # Remove existing symbolic links (in case they are dead).
            if (os.path.islink(destination)): os.remove(destination)
            # Skip existing files (copied in manually, do not delete).
            elif (os.path.exists(destination)): continue
            # Create a new symbolic link.
            os.symlink(source, destination)

    print()
    # Return the prepared working directory
    return build_dir, temp_dir

# ====================================================================

# Return True if a module should be rebuilt, False otherwise.
def should_rebuild_module(dependencies, module_name, module_directory):
    # Get the modification time of the Python package file.
    import os, sys, pkgutil, importlib
    # Get the last modification time of the module (if it exists already)
    # Make sure that the output directory is in the sys path so that
    # the time-check import loader will work correctly.
    sys.path.insert(0, module_directory)
    package = pkgutil.get_loader(module_name)
    if package is None: module_mod_time = 0
    else:               module_mod_time = os.path.getmtime(package.path)
    # Get the latest modification time of all source files.
    source_mod_time = max(map(os.path.getmtime, dependencies))
    # Return `True` if the source file has been modified since the
    # last construction of the module.
    if (module_mod_time <= source_mod_time):
        sys.path.pop(0)
        return True
    # If the module file is newer than the source file, try to import.
    try: importlib.import_module(module_name)
    except ImportError: return True # problem with the built module..
    finally: sys.path.pop(0)
    # The module successfully imported, no rebuild necessary.
    return False

# ====================================================================

# Save a configuration globally on this machine. To see the list of
# all configurations, call with no arguments.
def configure(*to_delete, **to_save):
    # Get the "home_directory" and the "config_file" for saving.
    import os, time, builtins
    from fmodpy.config import home_directory, config_file
    # Use the built-in print function, instead of the fmodpy one (default).
    from builtins import print
    # Otherwise, add the given configuration values to the config file.
    path = os.path.join(home_directory, config_file)
    # Read the existing file, if it's there.
    lines = []
    if (os.path.exists(path)):
        with open(path, "r") as f:
            lines = [l.strip() for l in f.readlines()]
    # If no arguments were given, print the current
    # configuration to standard output.
    if ((len(to_delete) == 0) and (len(to_save) == 0)):
        import fmodpy
        from fmodpy.config import load_config
        existing = {''.join([v.strip() for v in l.split("=")[:1]]) for l in lines}
        conf = load_config()
        # Collect the lines of the printout.
        lines = [f"fmodpy ({fmodpy.__version__}):"]
        finished_existing = False
        if (len(existing) > 0): lines += [f" # configuration declared in {path}"]
        for name in sorted(sorted(conf), key=lambda n: n not in existing):
            if (len(existing) > 0) and (name not in existing) and (not finished_existing):
                lines += [""," # default values"]
                finished_existing = True
            lines.append(f"  {name} = {str([conf[name]])[1:-1]}")
        # Print the output.
        print("\n".join(lines+[""]))
    else:
        # Overwrite the file, commenting out all the old stuff.
        with open(path, "w") as f:
            to_remove = {n for n in to_delete}.union(set(to_save))
            # Write the old contents, commented out.
            for l in lines:
                # If this variable is being overwritten, comment out the former.
                comment_out = any(v.strip() in to_remove for v in l.split("=")[:1])
                if comment_out: l = "# "+l
                # Add the line to the file (so there is a history).
                print(l, file=f)
            # Write the new contents.
            print("", file=f)
            print("# ",time.ctime(), file=f)
            for name in sorted(to_save):
                value = to_save[name]
                str_value = str([to_save[name]])[1:-1]
                print(f"{name} = {str_value}", file=f)
