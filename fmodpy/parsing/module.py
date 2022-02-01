from .code import Code
from . import parse_use, parse_implicit, parse_public, parse_private, \
    parse_argument, parse_interface, parse_type, parse_subroutine, \
    parse_function
from .subroutine import Subroutine
from .function import Function


# --------------------------------------------------------------------
class Module(Code):
    type = "MODULE"
    status = "PUBLIC"
    can_contain = [(parse_use, "uses"),
                   (parse_implicit, "implicit_none"),
                   (parse_public, "public"),
                   (parse_private, "private"),
                   (parse_type, "types"),
                   (parse_interface, "interfaces"),
                   (parse_argument, "arguments"),
                   (parse_subroutine, "subroutines"),
                   (parse_function, "functions"),
    ]


    # Produce a string representation of this code object.
    def __str__(self):
        out = f"{self.type} {self.name}\n"
        # Add documentation.
        if (len(self.docs.strip()) > 0):
            doc_lines = self.docs.strip().split("\n")
            for line in doc_lines: out += f"  {line}\n"
        # Add used modules.
        for line in sorted(self.uses): out += f"  {line}\n"
        for line in sorted(self.implicit_none): out += f"  {line}\n"
        # Add types.
        for t in self.types:
            for line in str(t).split("\n"):
                out += f"  {line}\n"
        # Add interfaces.
        if (len(self.interfaces) > 0): out += "\n"
        for i in self.interfaces:
            for line in str(i).split("\n"):
                out += f"  {line}\n"
        # Only add the space before arguments if types or
        # interfaces came before them.
        if ((max(len(self.interfaces),len(self.types)) > 0) and
            (len(self.arguments) > 0)): out += "\n"
        # Add arguments.
        for a in self.arguments:
            out += f"  {a}\n"
        # Add subroutines.
        for obj in self.subroutines:
            out += "\n"
            for line in str(obj).split("\n"):
                out += f"  {line}\n"
        # Add functions.
        for obj in self.functions:
            out += "\n"
            for line in str(obj).split("\n"):
                out += f"  {line}\n"
        # End the module.
        out += f"END {self.type} {self.name}\n"
        return out

    # Generate Fortran wrapper code that can call this Module's source code.
    def generate_fortran(self):
        # Generate the input signature.
        fortran_arguments = []
        for arg in self.arguments: fortran_arguments += arg.fort_input()
        lines = ['',f'{self.type} C_{self.name}']
        # Add the "USE" line.
        lines += self.uses
        if any((a.allocatable and a._is_output()) for a in self.arguments):
            lines += ['  USE ISO_FORTRAN_ENV, ONLY: INT64']
        # Enforce no implicit typing (within this code).
        lines += [f'  IMPLICIT NONE']
        lines += ['']

        # Declare all interfaces.
        for i in self.interfaces:
            if (len(i.subroutines) + len(i.functions) > 0):
                lines += ['']
                lines += ['  '+l for l in str(i).split("\n")]

        lines += ['']
        lines += ['CONTAINS']
        lines += ['']

        # Define all "getter" and "setter" subroutines for accessing
        # internal module (PUBLIC) attributes.
        for arg in self.arguments:
            lines += ['',f'  ! Getter and setter for {arg.name}.']
            lines += ['  '+l for l in arg.fort_getter()]
            lines += ['  '+l for l in arg.fort_setter()]

        # Declare all subroutines and functions.
        for code in self.subroutines + self.functions:
            lines += ['']
            lines += ['  ' + l for l in code.generate_fortran()]

        # Add the END line.
        lines += [f"END {self.type} C_{self.name}",'']

        return lines

    # Generate Python code.
    def generate_python(self, *args):
        # Add access points for the contained subroutines and functions.
        lines = [ '',
                 f'class {self.name.lower()}:',
                 f"    '''{self.docs}'''"]
        # Generate internal types.
        for t in self.types:
            lines += [ "    "+l for l in t.py_declare()]
        # Generate getter and setter for arguments.
        for arg in self.arguments:
            py_name = arg.name.lower()
            lines += ['']
            lines += [f"    # Declare '{py_name}'"]
            # Declare the "getter" for this module variable.
            lines += [ "    "+l for l in arg.py_getter() ]
            # Declare the "setter" for this module variable.
            lines += [ "    "+l for l in arg.py_setter() ]
            # Declare this as a property that has a getter and a setter.
            lines += [ "    "+l for l in arg.py_property() ]
        # Generate static methods for all internal routines.
        for code in self.subroutines + self.functions:
            lines += ['']
            lines += ['    '+l for l in code.generate_python(*args)]
        # Replace the class definition with a single instance of the class.
        lines += [ "",
                  f"{self.name.lower()} = {self.name.lower()}()"]
        return lines

    # Given a list of lines (of a source Fortran file), parse out this
    # module (assuming the first line is the first line *inside* of
    # this module).
    def parse(self, list_of_lines):
        self.lines += 1
        line = list_of_lines.pop(0).strip().split()
        # Remove any comments on the line that may exist.
        if ("!" in line): line = line[:line.index("!")]
        # Catch a potential parsing error.
        if (len(line) <= 1):
            from fmodpy.exceptions import ParseError
            raise(ParseError(f"Expected 'MODULE <NAME>', but the line did not contain the name of module.\n  {list_of_lines[0]}"))
        # Get the name of this module.
        self.name = line[1]
        # ------- Default parsing operations -------
        super().parse(list_of_lines)
        # ------------------------------------------
        # Get the set of things declared private.
        private = set(self.private)
        # Remove any instances of things that are declared private.
        if (len(private) > 0):
            for attr in ("arguments","interfaces","types","subroutines","functions"):
                codes = getattr(self,attr)
                to_remove = [i for (i,c) in enumerate(codes) if (c.name in private)]
                for i in reversed(to_remove): codes.pop(i)
                if (len(to_remove) > 0):
                    from fmodpy.config import fmodpy_print as print
                    print(f"  removed {len(to_remove)} from '{attr}' declared PRIVATE..")
        # Remove any things not declared public if this MODULE is private.
        if (self.status == "PRIVATE"):
            public = set(self.public)
            for attr in ("interfaces","types","subroutines","functions"):
                codes = getattr(self,attr)
                to_remove = [i for (i,c) in enumerate(codes) if (c.name not in public)]
                for i in reversed(to_remove): codes.pop(i)
                if (len(to_remove) > 0):
                    from fmodpy.config import fmodpy_print as print
                    print(f"  removed {len(to_remove)} from '{attr}' because this MODULE is PRIVATE..")
        # Make sure all "arguments" don't show intent.
        for a in self.arguments: a.show_intent = False
