from .code import Code
from . import parse_subroutine, parse_function, parse_module, parse_type

# --------------------------------------------------------------------
# Class for holding all relevant information in a Fortran file.
class Fortran(Code):
    docs = ""
    type = "FILE"
    needs_end = False
    allowed_unknown = True
    can_contain = [(parse_subroutine, "subroutines"),
                   (parse_function, "functions"),
                   (parse_module, "modules"),
                   (parse_type, "types"),
    ]

    # Create Fortran Subroutines with "BIND(C)" that are
    # accessible from C, translate arguments to Fortran, call the
    # Fortran code, and translates arguments back (if necessary).
    def generate_fortran(self):
        lines = ["! This automatically generated Fortran wrapper file allows codes",
                 "! written in Fortran to be called directly from C and translates all",
                 "! C-style arguments into expected Fortran-style arguments (with",
                 "! assumed size, local type declarations, etc.).",
                 ""]
        lines += super().generate_fortran()
        # Return the list of lines for this object (add an extra newline).
        from . import wrap_long_lines
        # Return the final Fortran file as a string.
        return "\n".join(wrap_long_lines(lines+['']))

    # Generate a string representation of the python code for this wrapper.
    def generate_python(self):
        # Define the header to this file. Some necessary variables are
        # left to be filled by formatting, namely:
        #    f_compiler          -- string
        #    shared_object_name  -- string
        #    f_compiler_args     -- string
        #    dependencies        -- list of strings
        #    module_name         -- string
        lines = ["'''This Python code is an automatically generated wrapper",
                 "for Fortran code made by 'fmodpy'. The original documentation",
                 "for the Fortran source code follows.",
                 "",
                 self.docs,
                 "'''",
                 '',
                 'import os',
                 'import ctypes',
                 'import numpy',
                 '',
                 '# --------------------------------------------------------------------',
                 '#               CONFIGURATION',
                 '# ', 
                 'fort_compiler = "{f_compiler}"',
                 'shared_object_name = "{shared_object_name}"',
                 'this_directory = os.path.dirname(os.path.abspath(__file__))',
                 'path_to_lib = os.path.join(this_directory, shared_object_name)',
                 'compile_options = "{f_compiler_args}"',
                 'ordered_dependencies = {dependencies}',
                 '# ',
                 '# --------------------------------------------------------------------',
                 '#               AUTO-COMPILING',
                 '#',
                 '# Try to import the existing object. If that fails, recompile and then try.',
                 'try:',
                 '    {module_name}_clib = ctypes.CDLL(path_to_lib)',
                 'except:',
                 '    # Remove the shared object if it exists, because it is faulty.',
                 '    if os.path.exists(shared_object_name):',
                 '        os.remove(shared_object_name)',
                 '    # Compile a new shared object.',
                 '    command = " ".join([fort_compiler, compile_options]',
                 '                        + ordered_dependencies + ["-o", path_to_lib])',
                 '    print("Running command")',
                 '    print("  ", command)',
                 '    # Run the compilation command.',
                 '    import subprocess',
                 '    subprocess.run(command, shell=True, cwd=this_directory)',
                 '    # Remove all ".mod" files that were created to reduce clutter.',
                 '    all_mods = [f for f in os.listdir(os.curdir) if f[-4:] == ".mod"]',
                 '    for m in all_mods: os.remove(m)',
                 '',
                 '# Import the shared object file as a C library with ctypes.',
                 'clib = ctypes.CDLL(path_to_lib)',
                 '',
                 '# --------------------------------------------------------------------',
                 '']
        # Add all the Python lines for the children of this Code.
        lines += super().generate_python()
        # Return the Python file as a string.
        return "\n".join(lines + [''])
