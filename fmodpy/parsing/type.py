from .code import Code
from .argument import Argument
from . import parse_argument

class TypeDeclaration(Code):
    type = "TYPE"
    can_contain = [(parse_argument, "arguments")]

    # Given a list of lines (of a source Fortran file), parse out this
    # TYPE (assuming the first line is the line declaring this
    # Subroutine).
    def parse(self, list_of_lines):
        # If there is a "(" then this doesn't declare a type, it is an argument.
        if ("(" in list_of_lines[0]): return
        # Otherwise, this declares a type.
        self.lines += 1
        # Parse the name of this subroutine out of the argument list.
        declaration_line = list_of_lines.pop(0).strip().split()
        print("declaration_line: ",declaration_line)
        assert(len(declaration_line) == 2)
        self.name = declaration_line[1]
        # ------- Default parsing operations -------
        super().parse(list_of_lines)
        # ------------------------------------------

    def __str__(self):
        # Begin type.
        out = f"{self.type} {self.name}\n" 
        # Add arguments.
        for a in self.arguments:
            temp = a.copy()
            temp.show_intent = False
            out += f"  {temp}\n"
        # End type.
        out += f"END {self.type} {self.name}"
        return out


class TypeArgument(Argument):
    type = "TYPE"
    c_types = {}
    default_singleton = 1
    kind_prefix = ""

    def py_declare(self):
        lines = []
