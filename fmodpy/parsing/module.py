from .code import Code
from . import parse_use, parse_implicit, parse_argument, \
    parse_interface, parse_type, parse_subroutine, parse_function
from .subroutine import Subroutine
from .function import Function


# --------------------------------------------------------------------
class Module(Code):
    type = "MODULE"
    can_contain = [(parse_use, "uses"),
                   (parse_implicit, "implicit_none"),
                   (parse_argument, "arguments"),
                   (parse_interface, "interfaces"),
                   (parse_type, "types"),
                   (parse_subroutine, "subroutines"),
                   (parse_function, "functions"),
    ]


    # Produce a string representation of this code object.
    def __str__(self):
        out = f"{self.starts_with} {self.name}\n"
        # Add documentation.
        if (len(self.docs.strip()) > 0):
            doc_lines = self.docs.strip().split("\n")
            for line in doc_lines: out += f"  {line}\n"
        # Add the "USE" line.
        for m in sorted(self.uses):
            if (len(self.uses[m]) == 0): out += "  USE {m}\n"
            else: out += f"  USE {m}, ONLY: {', '.join(self.uses[m])}\n"
        # Add all internal functions and subroutines (in the future
        # arguments, types, and interfaces too).
        if (len(self.contains) > 0): out += "\nCONTAINS\n"
        for code in self.contains:
            summary = str(code).strip().split("\n")
            out += "\n"
            for line in summary:
                out += f"  {line}\n"
        out += f"\nEND {self.starts_with} {self.name}"
        return out

    # Generate Fortran wrapper code that can call this Module's source code.
    def generate_fortran(self):
        lines = [f"{self.starts_with} C_{self.name}"]
        # Add the "USE" line.
        for m in sorted(self.uses):
            if (len(self.uses[m]) == 0): lines += ["  USE {m}"]
            else: lines += [f"  USE {m}, ONLY: {', '.join(self.uses[m])}"]
        # Add the "USE" line for the source module.
        lines += [f"  USE {self.name}",
                  "  ! All variables should be defined, using no implicits.",
                  "  IMPLICIT NONE"]
        # Add custom types
        # Add interfaces (for PROCEDURE declarations).
        lines += ['','CONTAINS']
        # Add all the contained subroutines and functions.
        for code in self.contains:
            lines += ['  '+l for l in code.generate_fortran()]
        lines += [f"END {self.starts_with} c_{self.name}"]
        return lines

    # Evaluate the sizes of all arguments to the the funnctions and
    # subroutines for this module.
    def eval_sizes(self, build_dir):
        for code in self.contains: code.eval_sizes(build_dir)

    # Generate Python-callable Cython code that calls
    def generate_cython(self):
        # Add access points for the contained subroutines and functions.
        lines = []
        for code in self.contains: lines += code.generate_cython() + ['']
        return lines

    # Given a list of lines (of a source Fortran file), parse out this
    # module (assuming the first line is the first line *inside* of
    # this module).
    def parse(self, list_of_lines):
        raise(NotImplementedError)
        known_public = []
        known_private = []
        default_list = known_public
        # Check for documentation.
        while (len(list_of_lines) > 0) and (list_of_lines[0].strip()[:1] == "!"):
            self.docs += "\n" + list_of_lines.pop(0)
        self.docs = self.docs.strip()
        comments = ""
        # Loop until the end of this module.
        while True:
            # Raise error if the code ends unexpectedly.
            if (len(list_of_lines) == 0):
                from fmodpy.exceptions import FortranError
                raise(FortranError("File ended without observing 'END {self.starts_with}'."))
            # Read the next line.
            line = list_of_lines[0].strip().split()
            # Cycle empty lines.
            if (len(line) == 0):
                list_of_lines.pop(0)
                comments = ""
                continue
            # Check if this is a comment (line should not be empty).
            if (line[0] == "!"):
                comments += "\n" + list_of_lines.pop(0)
                continue
            # Check for END of this module.
            if (line[0] == "END"):
                list_of_lines.pop(0)
                # **** WHILE LOOP TERMINATION ****
                if parse_end(line, self.starts_with, self.name): break
                continue
            # Check for a USE statement.
            if (line[0] == "USE"):
                list_of_lines.pop(0)
                module, names = parse_use(line)
                self.uses[module] = self.uses.get(module,[]) + names
                continue
            # Check for PRIVATE declarations.
            if (line[0] == "PRIVATE"):
                list_of_lines.pop(0)
                if ":" in line:
                    # Grab the list of names after the "::".
                    line = line[3:]
                    while ("," in line): line.remove(",")
                    if (len(line) == 0):
                        from fmodpy.exceptions import FortranError
                        raise(FortranError("Expected 'PRIVATE ::' followed by comma separated names, but got '{list_of_lines[0]}'."))
                    # Some specific subroutines and functions are declared private.
                    known_private += line
                else:
                    # If the module is declared private then change the default list.
                    default_list = known_private
                continue
            # Check for PRIVATE declarations.
            if (line[0] == "PUBLIC"):
                list_of_lines.pop(0)
                if ":" in line:
                    # Grab the list of names after the "::".
                    line = line[3:]
                    while ("," in line): line.remove(",")
                    if (len(line) == 0):
                        from fmodpy.exceptions import FortranError
                        raise(FortranError("Expected 'PUBLIC ::' followed by comma separated names, but got 'list_of_lines[0]'."))
                    # Some specific subroutines and functions are declared private.
                    known_public += line
                else:
                    # If the module is declared private then change the default list.
                    default_list = known_public
                continue
            # Check for SUBROUTINE declaration.
            instance = Subroutine(list_of_lines, comments, parent=self)
            if (instance.lines > 0):
                if instance.name not in known_private:
                    self.contains.append( instance )
                comments = ""
                continue
            # Check for FUNCTION declaration.
            instance = Function(list_of_lines, comments, parent=self)
            if (instance.lines > 0):
                self.contains.append( instance )
                comments = ""
                continue
            # Nothing match, 
            line = list_of_lines.pop(0)
            from fmodpy.config import fmodpy_print as print
            print(f"no match for '{line}'")
