from . import parse_subroutine, parse_function
from .code import Code

# PROCEDURE cannot have "INTENT".
# 
# All passed procedure arguments, will be defined as 
#   'ABSTRACT INTERFACE's and declared at the front of the 
#   definitions for a routine. Since they are passed, they must
#   be procedures with the BIND(C) attribute enabled.


# For parsing purposes, an Interface looks like a Module.
class Interface(Code):
    type = "INTERFACE"
    named_end = False
    # prefixes = ["ABSTRACT"] # <- need another type for abstrac ones.
    can_contain = [(parse_subroutine, "subroutines"),
                   (parse_function, "functions")
    ]

    # Produce a string representation of this Subroutine object.
    def __str__(self):
        # Print out header line.
        out =  "INTERFACE\n"
        for f in self.subroutines:
            for line in str(f).split("\n"):
                out += "  "+line+"\n"
        if (len(self.subroutines) > 0): out += "\n"
        for f in self.functions:
            for line in str(f).split("\n"):
                out += "  "+line+"\n"
        out += "END INTERFACE"
        # Return the whole interface.
        return out

    # Given a list of lines (of a source Fortran file), parse out this
    # Function (assuming the first line is the first line *inside* of
    # this Function).
    def parse(self, list_of_lines):
        # Pop out the first line, it's not needed.
        list_of_lines.pop(0)
        # Use standard parsing code (looks for subroutines and functions).
        super().parse(list_of_lines)
