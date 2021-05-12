from .subroutine import Subroutine

# --------------------------------------------------------------------
class Function(Subroutine):
    type = "FUNCTION"
    result = None

    # Produce a string representation of this Subroutine object.
    def __str__(self):
        # Print out header line.
        arg_names = [a.name for a in self.arguments if (a.name != self.result)]
        out = f"{self.type} {self.name}({', '.join(arg_names)})"
        if (self.result != self.name): out += f" RESULT({self.result})\n"
        else:                          out += "\n"
        # Use SUBROUTINE string method for the rest (strip SUBROUTINE).
        out += "\n".join(super().__str__().split("\n")[1:])
        return out

    # Given a list of lines (of a source Fortran file), parse out this
    # Function (assuming the first line is the first line *inside* of
    # this Function).
    def parse(self, list_of_lines):
        # Pre-fetch the "result" and modify the first line so that it
        # can be parsed correctly by the Subroutine.parse function.
        declaration_line = list_of_lines[0].strip().split()
        arg_start = declaration_line.index("(")
        arg_end = declaration_line.index(")")
        argument_names = declaration_line[arg_start+1:arg_end]
        while "," in argument_names: argument_names.remove(",")
        name = declaration_line[:arg_start][-1]
        rest_of_line = declaration_line[arg_end+1:]
        # Get the result from the end of the line.
        if ("RESULT" in rest_of_line):
            if ("(" not in rest_of_line) or (")" not in rest_of_line):
                from fmodpy.exceptions import ParseError
                raise(ParseError(f"Found 'RESULT' but no '(<name>)' on line.\n{list_of_lines[0].strip()}"))
            self.result = ''.join(rest_of_line[rest_of_line.index("(")+1:
                                               rest_of_line.index(")")])
        else: self.result = name
        argument_names.append( self.result )
        list_of_lines[0] = f"SUBROUTINE {name} ( {' , '.join(argument_names)} )"
        # ------------------------------------------------------------
        # Use Subroutine parse code.
        super().parse(list_of_lines)
        # ------------------------------------------------------------
        # Find the result in the identified arguments, make sure its
        # intent is not declared (it will make Fortran compilers angry).
        for arg in self.arguments:
            if (arg.name == self.result):
                arg.intent = "OUT"
                arg.show_intent = False
                break
        else:
            from fmodpy.config import fmodpy_print as print
            print()
            print("Did not find output argument in parsed list..")
            print(self.result)
            print()
            raise(NotImplementedError)

