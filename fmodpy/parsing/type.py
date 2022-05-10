from .code import Code
from .argument import Argument
from . import parse_argument

STRUCT_DEFAULT_VALUE = 0
CONTENTS_DECLARATION = '("{n}", {t})'
FIELD_DECLARATION = 'self.{n} = kwargs.get("{n}", getattr(value, "{n}", value))'
STRUCTURE_DEFINITION = '''
# This defines a C structure that can be used to hold this defined type.
class {name}(ctypes.Structure):
    # (name, ctype) fields for this structure.
    _fields_ = [{fields}]
    # Define an "__init__" that can take a class or keyword arguments as input.
    def __init__(self, value={default}, **kwargs):
        # From whatever object (or dictionary) was given, assign internal values.
        {field_declarations}
    # Define a "__str__" that produces a legible summary of this type.
    def __str__(self):
        s = []
        for (n, t) in self._fields_:
            s.append( n + "=" + str(getattr(self,n)) )
        return "{name}[" + ", ".join(s) + "]"
    # Define an "__eq__" method that checks equality of all fields.
    def __eq__(self, other):
        for (n, t) in self._fields_:
            if (getattr(self, n) != getattr(other, n, None)):
                return False
        return True
'''


class TypeDeclaration(Code):
    type = "TYPE"
    can_contain = [(parse_argument, "arguments")]

    # Given a list of lines (of a source Fortran file), parse out this
    # TYPE (assuming the first line is the line declaring this
    # Subroutine).
    def parse(self, list_of_lines):
        # If there is "TYPE (" then this doesn't declare a type, it is an argument.
        if ("(" == list_of_lines[0][5:6]): return
        # Otherwise, this declares a type.
        self.lines += 1
        if ('BIND ( C )' not in list_of_lines[0]):
            from fmodpy.exceptions import NotSupportedError
            raise NotSupportedError(f"\n  Fortran derived types must be C compatible and include 'BIND(C)' to be wrapped.\n  Encountered this issue when parsing the following line:\n    {list_of_lines[0]}")
        # Parse the name of this subroutine out of the argument list.
        declaration_line = list_of_lines.pop(0).strip().split()
        assert (':' in declaration_line), "Expected ':' to exist in TYPE declaration line:\n  {' '.join(declaration_line)}"
        colon_index = -declaration_line[::-1].index(':')-1
        declaration_line = declaration_line[colon_index+1:]
        # Extract any "arguments" to the type.
        argument_names = set()
        #   Assume the format "TYPE :: name"
        #                  or "TYPE :: name(arg1, arg2, ...)"
        if ('(' in declaration_line):
            self.name = declaration_line.pop(0)
            argument_names = {
                token for token in declaration_line
                if token not in {'(', ',', ')'}
            }
        else:
            self.name = declaration_line[-1]
        # ------- Default parsing operations -------
        super().parse(list_of_lines)
        # ------------------------------------------
        # Disable showing "INTENT" on all arguments of this TYPE.
        for arg in self.arguments:
            arg.show_intent = False
        # Verify that all arguments are defined and permissable.
        if (len(argument_names) > 0):
            for arg in self.arguments:
                if (arg.name in argument_names):
                    if (arg.type_kind):
                        from fmodpy.exceptions import NotSupportedError
                        raise(NotSupportedError(
                            "\nDerived types with KIND parameters are not supported yet."+
                            "\n  Consider posting a simplified example with your motivation to"+
                            "\n  https://github.com/tchlux/fmodpy/issues"
                        ))
                    elif (not arg.type_len):
                        raise(NotImplemented())
                    argument_names.remove(arg.name)
            assert (len(argument_names) == 0), f'TYPE arguments {argument_names} were not defined within the type.'


    # Declare the lines that create the corresponding Struct for this type.
    # TODO: Treat the type like it is a c function being called, include
    #       size parameters for arrays (when it is not C compatible).
    def py_declare(self):
        # Get the names and types necessary to declare a C-compatible struct.
        name = self.name
        names = [arg.name for arg in self.arguments]
        types = [arg.c_type for arg in self.arguments]
        struct = STRUCTURE_DEFINITION.format(
            name = name,
            default = STRUCT_DEFAULT_VALUE,
            fields = ", ".join([CONTENTS_DECLARATION.format(n=n.lower(), t=t)
                                for (n,t) in zip(names, types)]),
            field_declarations = "\n        ".join([FIELD_DECLARATION.format(n=n.lower())
                                                    for n in names])
        )
        # Return the list of lines that defines this struct.
        return struct.split("\n")

    # Declare the lines that create this type in Fortran.
    def fort_declare(self):
        return str(self).split("\n")

    def __str__(self):
        # Define the arguments to the type (for constructing its internals).
        type_args = [a.name for a in self.arguments if a.type_len]
        if (len(type_args) > 0):
            args = f"(" + ", ".join(type_args) + ")"
        else:
            args = ""
        # Define the type.
        out = f"{self.type}, BIND(C) :: {self.name}{args}\n" 
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

    # For a type, the "c_type" in python is the type name (assuming
    #  the struct was defined in an accessible scope).
    @property
    def c_type(self):
        return self.kind

    # TODO: Make both the type and a C compatible equivalent.
    # def fort_declare(self):

    # TODO: Copy the input into the true Fortran type.
    # def fort_before(self): 

    # TODO: Copy the result from the call outputs into the C inputs.
    # def fort_after(self): 
