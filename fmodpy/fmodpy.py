# fmodpy is an automatic fortran wrapper for python.
# 
# This program is designed to processes a source fortran file into the
# following structure and wrap all contained code into a python module
# 
#   fimport             -- Given a path to a Fortran source file,
#                          wrap it and return a Python module.
#   make_python_wrapper -- Given a source Fortran file, a build directory,
#                          and a name: generate a Fortran wrapper code
#                          that is callable from C, and a Cython wrapper
#                          code that is callable from Python.
#   make_python_module  -- Given a wrapper Fortran file and a wrapper
#                          Cython file, compile a Python module.
# 
# 
# For configuration and future usage of `fmodpy` on a single machine,
# use the following function.
# 
#   configure -- Three behaviors related to `fmodpy` configuration:
#                  no arguments -> Print current configuration and return.
#                  str, *args -> Remove these configurations from global config file.
#                  key=value, **kwargs -> Assign this configuration in global config file.
# 

# Overwrite the "print" function to add "FMODPY" specific printouts.
from fmodpy.config import fmodpy_print as print


# Function for automating the entire fortran<->python wrapping
#   process. This is the top-level function built for standard usage.
#   This function automatically checks the modification times of the
#   output module as well as the source fortran in order to only
#   re-compile when necessary. Automatically wraps fortran, makes
#   symbolic links to all files in the source directory as potential
#   dependencies, generates a wrapper in Fortran, generates a wrapper
#   in Cython, then compiles all of the above into a single Python
#   module that can be imported (which is also returned).
# 
#  INPUTS:
#    input_fortran_file -- str, relative or absolute path to fortran source.
# 
#  OPTIONAL:
#    name       -- str, name of output python module. Defaults
#                  to be `before_dot(os.path.basename(input_fortran_file))`
#    build_dir  -- str, the directory to store the wrapper code for
#                  `input_fortran_file`. If provided, it is
#                  assumed that the directory should not be
#                  removed automatically, otherwise a temporary
#                  directory is created, used, and deleted.
#    output_dir -- str, the directory to store the output python
#                   module. Defaults to `os.getcwd()`.
#    link_files -- list of str, paths to `.o` files that should be
#                  included when linking the final module, useful
#                  when dependencies have separate `Make` scripts
#                  and/or specific compilation instructions.
#    **kwargs   -- Any configuration options relevant to this
#                  compilation passed as keyword arguments, 
#                  see `help(fmodpy)` for more information, or
#                  run again with 'verbose=True' to see options.
# 
#   KEYWORD OPTIONS:
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
            output_dir=None, link_files=None, **kwargs):
    # Import parsing functions.
    from fmodpy.parsing import before_dot, legal_module_name
    from fmodpy.config import run, load_config, \
        FORT_WRAPPER_EXT, CYTHON_EXT
    # Import "os" for os operations and "importlib" for importing a module.
    import os, sys, shutil, importlib

    # Configure this runtime of fmodpy 
    pre_config = load_config() # <- gets the configuration dictionary
    if (len(kwargs) > 0): load_config(**kwargs) # <- assigns given configuration
    # Import some locally used settings.
    from fmodpy.config import wrap, rebuild, autocompile

    # Print the configuration (when verbose).
    print("_"*70)
    print("fimport")
    print()
    print("fmodpy configuration:")
    c = load_config()
    for n in sorted(c): print(f"  {n} = {str([c[n]])[1:-1]}")
    if (len(c) > 0): del n
    del c

    # Set the default output directory
    if (output_dir is None): output_dir = os.getcwd()
    output_dir = os.path.abspath(output_dir)

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
                         " must match the regexp `^[a-zA-z_]+"+
                         "[a-zA-Z0-9_]*`. Set the name with:\n"+
                         " fmodpy.fimport(<file>, name='<legal name>')"+
                         " OR\n $ fmodpy <file> name=<legal_name>")))

    # Determine whether or not the module needs to be rebuilt.
    should_rebuild = rebuild or should_rebuild_module(
        source_path, name, output_dir)
    if not should_rebuild:
        print()
        print("No new modifications to '%s' module, exiting."%(name))
        print("_"*70)
        return importlib.import_module(name)

    # Generate the names of files that will be created by this
    # program, so that we can copy them to an "old" directory if necessary.
    fortran_wrapper_file = name+FORT_WRAPPER_EXT
    cython_wrapper_file = name+CYTHON_EXT
    build_dir_name = build_dir if (build_dir is not None) else "temporary directory"
    bar_width = 23 + max(map(len,(source_dir, source_file, name, build_dir_name,
                                  fortran_wrapper_file, cython_wrapper_file, output_dir)))
    print()
    print("="*bar_width)
    print("Input file directory: ",source_dir)
    print("Input file name:      ",source_file)
    print("Base module name:     ",name)
    print("Using build dir:      ",build_dir_name)
    print("  fortran wrapper:    ",fortran_wrapper_file)
    print("  cython wrapper:     ",cython_wrapper_file)
    print("Output file directory:",output_dir)
    print("="*bar_width)
    print()
 
    # Prepare the build directory, link in the necessary files.
    build_dir, temp_dir = prepare_build_directory(source_dir, build_dir)

    # Initialize the list of link files if they were not given.
    if (link_files is None): link_files = []
    # Automatically compile fortran files.
    if autocompile:
        print("Attempting to autocompile..")
        built, failed = autocompile_files(build_dir)
        if (len(built) > 0):  print("  succeeded:", built)
        if (len(failed) > 0): print("  failed:   ", failed)
        link_files = link_files + built
        source_object = before_dot(source_file)+".o"
        if (source_object not in link_files): link_files += [source_object ]
        # If there are still no link files, automatically add all present ".o" files.
        if (len(link_files) == 0):
            link_files += [f for f in os.listdir(build_dir) if (f[-2:] == ".o")]


    # Write the wrappers to the files so they can be compiled.
    fortran_wrapper_path = os.path.join(build_dir,fortran_wrapper_file)
    cython_wrapper_path = os.path.join(build_dir,cython_wrapper_file)
    # Check for the existence of the wrapper files.
    fortran_wrapper_exists = os.path.exists(fortran_wrapper_path)
    cython_wrapper_exists = os.path.exists(cython_wrapper_path)
    # Generate the wrappers for going from python <-> fortran.
    if (wrap or (not fortran_wrapper_exists) or (not cython_wrapper_exists)):
        fortran_wrapper, cython_wrapper = make_python_wrapper(
            source_path, build_dir, name)
    # Write the wrapper files if this program is supposed to.
    if (not fortran_wrapper_exists) or wrap:
        with open(fortran_wrapper_path, "w") as f: f.write( fortran_wrapper )
    if (not cython_wrapper_exists) or wrap:
        with open(cython_wrapper_path, "w") as f: f.write( cython_wrapper )

    # Build the python module by compiling all components
    module_path = make_python_module(name, build_dir, fortran_wrapper_path,
                                     cython_wrapper_path, link_files)
    # Remove the old module if it already exists.
    final_module_path = os.path.join(output_dir, os.path.basename(module_path))

    # Move the compiled module to the output directory.
    print("Moving from:", f"  {module_path}", "to", f"  {final_module_path}", sep="\n")
    if not (module_path == final_module_path):
        # Remove the existing wrapper if it exists.
        if os.path.exists(final_module_path):
            print(f" removing existing (outdated) copy of '{name}'..")
            os.remove(final_module_path)
        # Move the compiled wrapper to the destination.
        try: shutil.move(module_path, final_module_path)
        except FileNotFoundError: pass

    print(f"Finished making module '{name}'.\n")

    # Clean up the the temporary directory if one was created.
    if temp_dir is not None:
        temp_dir.cleanup()
        del temp_dir

    # Re-configure 'fmodpy' to work the way it did before this execution.
    if (len(kwargs) > 0): load_config(**pre_config)

    # Import the module after deleting any copies already in memory.
    sys.path.insert(0, output_dir)
    module = importlib.import_module(name)
    module = importlib.reload(module)
    sys.path.pop(0)
    # Return the module to be stored as a variable.
    return module

# ====================================================================

# Function for compiling and creating a python module out of the
# prepared fortran + wrapper code.
def make_python_module(mod_name, build_dir, fortran_wrapper_path,
                       cython_wrapper_path, link_files):
    import os, sys, importlib
    # Load the configuration (in case this function is called directly).
    from fmodpy.config import load_config
    load_config()
    # Import fmodpy configurations variables.
    from fmodpy.config import run, f_compiler, f_compiler_args, c_compiler, \
        c_compiler_args, c_linker, c_linker_args, python_link_command
    from fmodpy.parsing import before_dot

    # Compile the fortran wrapper.
    fortran_wrapper_file = os.path.basename(fortran_wrapper_path)
    print(f"Compiling '{fortran_wrapper_file}'.. ", end="")
    wrap_code, stdout, stderr = run([f_compiler]+f_compiler_args+[fortran_wrapper_file],
                                    cwd=build_dir)
    if wrap_code != 0:
        print("failed.\n")
        print("\n".join(stderr))
        from fmodpy.exceptions import CompileError
        raise(CompileError("\nError when compiling Fortran wrapper (fmodpy bug?)\n"))
    print("success.")

    # Add the compiled Fortran wrapper file to the list of linked files.
    fortran_wrapper_file_object = before_dot(fortran_wrapper_file) + ".o"
    if fortran_wrapper_file_object not in link_files:
        link_files.append( fortran_wrapper_file_object )

    print("Compiling extension module using setup..")
    print("="*70)
    print()

    # Create a simple Python file that will build the extension.
    lines = [
        "from distutils.core import setup",
        "from distutils.extension import Extension",
        "from Cython.Build import cythonize",
        "import numpy",
        "",
        "mod_name = '"+mod_name+"'",
        "cython_wrapper_file = '"+os.path.basename(cython_wrapper_path)+"'",
        "c_compiler_args = "+str(c_compiler_args),
        "c_linker_args = "+str(c_linker_args),
        "link_files = "+str(link_files),
        "",
        "# Generate the extension module",
        "ext_modules = [ Extension(mod_name, [cython_wrapper_file],",
        "                          extra_compile_args=c_compiler_args,",
        "                          extra_link_args=c_linker_args + link_files,",
        "                          include_dirs = [numpy.get_include()])]",
        ""
        "# Compile the '.pyx' Cython file into a '.c' file, load as an extension.",
        "extensions = cythonize(ext_modules)",
        "dist = setup(name=mod_name, ext_modules=extensions)",
        ""
    ]

    # Write the file to the build directory.
    py_build_file = "build_"+mod_name+".py"
    with open(os.path.join(build_dir,py_build_file), "w") as f:
        f.write("\n".join(lines))

    # Set the shell environment variables (setting C linker and C compiler).
    environment = os.environ.copy()
    environment["LDSHARED"] = " ".join([c_linker]+python_link_command)
    if (c_compiler is not None): environment["CC"] = c_compiler
    # Build this module by calling Python with the setup arguments to
    # the command line. Capture output to check for success.
    code, stdout, stderr = run(
        [sys.executable, py_build_file, "build_ext", "--inplace"],
        cwd=build_dir, env=environment)
    # Raise an exception if the module failed to build.
    if (code != 0):
        from fmodpy.exceptions import BuildAndLinkError
        raise(BuildAndLinkError("\n".join(stderr)))
    print("\n".join(stdout))
    print("="*70)
    print("Done building module. Testing import..")

    # Manipulate the path to test importing the module, make sure it
    # will appear first in the path.
    sys.path.insert(0, build_dir)
    try:
        # Try loading the newly created module (reload to ensure if it
        # was already present it gets reloaded).
        module = importlib.import_module(mod_name)
        module = importlib.reload(module)
        module_path = os.path.abspath(module.__file__)
    except ImportError as exc:
        from fmodpy.exceptions import LinkError
        raise(LinkError("\n\n"+str(exc)+"\n\nUnable to successfully import module.\n Perhaps the "+
                        "relevant compiled fortran files were not available? See message above."))
    # Revert the system path to its original state.
    finally: sys.path.pop(0)
    print("Successfully imported module.")
    if (not os.path.exists(module_path)): raise(Exception("File disappeared, fmodpy bug?"))
    return module_path

# ====================================================================

# Given a file name to a source python file and a build directory,
# make a python wrapper that wraps the given fortran file as if it were
# a python module. Allow for the import of all modules (with functions),
# and plain funcitons / subroutines listed in the fortran file.
def make_python_wrapper(source_file, build_dir, module_name):
    import os
    from fmodpy.parsing import simplify_fortran_file, after_dot
    from fmodpy.parsing.file import Fortran
    # Make a simplified version of the fortran file that only contains
    # the relevant syntax to defining a wrapper in python.
    is_fixed_format = after_dot(source_file) == "f"
    simplified_file = simplify_fortran_file(source_file, is_fixed_format)
    # Parse the simplified fortran file into an abstract syntax.
    abstraction = Fortran(simplified_file)
    print("-"*70)
    print(abstraction)
    print("-"*70)
    # Generate the C <-> Fortran code.
    fortran_file = abstraction.generate_fortran()
    # Evaluate the sizes of all the Fortran variables.
    abstraction.eval_sizes(build_dir)
    # Generate the python <-> C code.
    cython_file = abstraction.generate_cython()
    # Return the two files that can be used to construct the wrapper.
    return fortran_file, cython_file

# ====================================================================

# Given a build directory (containing some fortran files), repeatedly
# attempt to compile all files in the build directory until there are
# no successfully compiled files.
def autocompile_files(build_dir):
    import os
    from fmodpy.parsing import after_dot
    # Get configuration parameters.
    from fmodpy.config import run, f_compiler, f_compiler_args, \
        GET_SIZE_PROG_FILE
    # Get the list of existing object files.
    existing_o_files = {f for f in os.listdir(build_dir) if after_dot(f) == "o"}
    # Try and compile the rest of the files (that might be fortran) in
    # the working directory in case any are needed for linking.
    should_compile = []
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
    successes = [None]
    failed = []
    # Continue rounds until (everything compiled) or (no success)
    print()
    while (len(should_compile) > 0) and (len(successes) > 0):
        successes = []
        for f in should_compile:
            # Try to compile all files that have "f" in the extension
            print(f"Compiling '{f}'.. ")
            print(f" {' '.join([f_compiler]+f_compiler_args+[f])}")
            code, stdout, stderr = run([f_compiler]+f_compiler_args+[f],cwd=build_dir)
            if code == 0:
                successes.append(f)
                print(" success.")
            else:
                # Record failed compilations.
                if f not in failed: failed.append(f)
                print(" failed.")
                if (max(len(stdout), len(stderr)) > 0): print('-'*70)
                if len(stdout) > 0:
                    print("STANDARD OUTPUT:")
                    print("\n".join(stdout))
                if len(stderr) > 0:
                    print("STANDARD ERROR:")
                    print("\n".join(stderr))
                if (max(len(stdout), len(stderr)) > 0): print('-'*70)
        # Remove the files that were successfully compiled from
        # the list of "should_compile" and the list of "failed".
        for f in successes:
            should_compile.remove(f)
            if f in failed: failed.remove(f)

    # Log the files that failed to compile.
    for f in should_compile: print(f"Failed to compile '{f}'.")
    # Return the list of object files created during compilation.
    built = [f for f in os.listdir(build_dir)
             if (f[-2:] == ".o") and f not in existing_o_files] 
    return built, failed


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
            source = os.path.join(source_dir,f)
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
def should_rebuild_module(source_path, module_name, module_directory):
    # Get the modification time of the Python package file.
    import os, sys, pkgutil, importlib
    # Get the last modification time of the module (if it exists already)
    # Make sure that the output directory is in the sys path so that
    # the time-check import loader will work correctly.
    sys.path.insert(0, module_directory)
    package = pkgutil.get_loader(module_name)
    if package is None: module_mod_time = 0
    else:               module_mod_time = os.path.getmtime(package.path)
    # Get the modification time of the source file.
    source_mod_time = os.path.getmtime(source_path)
    # Return `True` if the source file has been modified since the
    # last construction of the module.
    if (module_mod_time <= source_mod_time):
        sys.path.pop(0)
        return True
    # If the module file is newer than the source file, try to import.
    try: importlib.import_module(module_name)
    except ImportError: return True
    finally: sys.path.pop(0)
    # The module successfully imported, no rebuild necessary.
    return False

# ====================================================================

# Save a configuration globally on this machine.
def configure(*to_delete, **conf):
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
        with open(path, "r") as f:lines = [l.strip() for l in f.readlines()]
    # If no arguments were given, print the current
    # configuration to standard output.
    if (len(conf) == 0):
        import fmodpy
        from fmodpy.config import load_config
        existing = {''.join([v.strip() for v in l.split("=")[:1]]) for l in lines}
        conf = load_config()
        # Collect the lines of the printout.
        lines = [f"fmodpy ({fmodpy.__version__}):"]
        finished_existing = False
        if (len(existing) > 0): lines += ["",f" declared in {path}",""]
        for name in sorted(sorted(conf), key=lambda n: n not in existing):
            if (len(existing) > 0) and (name not in existing) and said_default:
                lines += [""," default values",""]
                finished_existing = False
            lines.append(f"  {name} = {str([conf[name]])[1:-1]}")
        # Print the output.
        print("\n".join(lines+[""]))
        # End the function.
        return 
    # Overwrite the file, commenting out all the old stuff.
    with open(path, "w") as f:
        to_remove = {n for n in to_delete}.union(set(conf))
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
        for name in sorted(conf):
            print(f"{name} = {str([conf[name]])[1:-1]}", file=f)
