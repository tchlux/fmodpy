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

    # Generate a string describing this file.
    def __str__(self):
        out = f"{self.type} {self.name}"
        for (_, name) in self.can_contain:
            if (len(getattr(self, name)) > 0): out += "\n"
            for obj in getattr(self,name):
                for line in str(obj).split("\n"):
                    out += "  "+line+"\n"
        return out


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
                 'import platform',
                 'import numpy',
                 '',
                 '# --------------------------------------------------------------------',
                 '#               CONFIGURATION',
                 '# ', 
                 '_verbose = {verbose_module}',
                 '_fort_compiler = "{f_compiler}"',
                 '_shared_object_name = "{shared_object_name}." + platform.machine() + ".so"',
                 '_this_directory = os.path.dirname(os.path.abspath(__file__))',
                 '_path_to_lib = os.path.join(_this_directory, _shared_object_name)',
                 '_compile_options = {f_compiler_args}',
                 '_ordered_dependencies = {dependencies}',
                 '_symbol_files = {symbol_files}'
                 '# ',
                 '# --------------------------------------------------------------------',
                 '#               AUTO-COMPILING',
                 '#',
                 '# Try to import the prerequisite symbols for the compiled code.',
                 'for _ in _symbol_files:',
                 '    _ = ctypes.CDLL(os.path.join(_this_directory, _), mode=ctypes.RTLD_GLOBAL)',
                 '# Try to import the existing object. If that fails, recompile and then try.',
                 'try:',
                 '    clib = ctypes.CDLL(_path_to_lib)',
                 'except:',
                 '    # Remove the shared object if it exists, because it is faulty.',
                 '    if os.path.exists(_shared_object_name):',
                 '        os.remove(_shared_object_name)',
                 '    # Compile a new shared object.',
                 '    _command = " ".join([_fort_compiler] + _compile_options + ["-o", _shared_object_name] + _ordered_dependencies)',
                 '    if _verbose:',
                 '        print("Running system command with arguments")',
                 '        print("  ", _command)',
                 '    # Run the compilation command.',
                 '    import subprocess',
                 '    subprocess.run(_command, shell=True, cwd=_this_directory)',
                 '    # Import the shared object file as a C library with ctypes.',
                 '    clib = ctypes.CDLL(_path_to_lib)',
                 '# --------------------------------------------------------------------',
                 '']
        # Add all the Python lines for the children of this Code.
        lines += super().generate_python()
        # Return the Python file as a string.
        return "\n".join(lines + [''])
