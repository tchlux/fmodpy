from .code import Code
from . import parse_code, parse_argument, parse_use, parse_type, \
    parse_implicit, parse_interface, parse_subroutine, parse_function

# ALLOCATABLE is not allowed to go from C -> Fortran. That means
#  INTENT(IN) is not allowed generally, and inside of a passed
#  PROCEDURE, INTENT(OUT) is not allowed.

# --------------------------------------------------------------------
class Subroutine(Code):
    type = "SUBROUTINE"
    prefixes = ["RECURSIVE"]
    can_contain = [(parse_use, "uses"),
                   (parse_implicit, "implicit_none"),
                   (parse_type, "types"),
                   (parse_interface, "interfaces"),
                   (parse_argument, "arguments"),
    ]
    will_ignore = [parse_subroutine, parse_function]

    # Produce a string representation of this Subroutine object as it
    # appears in the Fortran source (NOT as the wrapper will appear).
    def __str__(self):
        # Print out header line.
        out = f"{self.type} {self.name}({', '.join([a.name for a in self.arguments])})\n"
        # Add documentation.
        if (len(self.docs.strip()) > 0):
            doc_lines = self.docs.strip().split("\n")
            for line in doc_lines: out += f"  {line}\n"
        # Add used modules.
        for line in sorted(self.uses): out += "  "+line+"\n"
        for line in sorted(self.implicit_none): out += "  "+line+"\n"
        # Add types
        if (len(self.types) > 0): out += "\n"
        for t in self.types:
            for line in str(t).split("\n"):
                out += "  "+line+"\n"
        # Add interfaces.
        if (len(self.interfaces) > 0): out += "\n"
        for i in self.interfaces:
            for line in str(i).split("\n"):
                out += "  "+line+"\n"
        # Only add the space before arguments if types or
        # interfaces came before them.
        if ((max(len(self.interfaces),len(self.types)) > 0) and
            (len(self.arguments) > 0)): out += "\n"
        # Add arguments.
        for a in self.arguments: out += f"  {a}\n"
        # End the subroutine.
        out += f"END {self.type} {self.name}"
        return out

    # Given a list of lines (of a source Fortran file), parse out this
    # Subroutine (assuming the first line is the line declaring this
    # Subroutine).
    def parse(self, list_of_lines):
        self.lines += 1
        # Parse the name of this subroutine out of the argument list.
        declaration_line = list_of_lines.pop(0).strip().split()
        arg_start = declaration_line.index("(")
        arg_end = declaration_line.index(")")
        argument_names = declaration_line[arg_start+1:arg_end]
        argument_order = argument_names.copy()
        while "," in argument_names: argument_names.remove(",")
        self.name = declaration_line[:arg_start][-1]
        # ------- Default parsing operations -------
        super().parse(list_of_lines)
        # ------------------------------------------
        # Remove all declarations that are not arguments.
        to_remove = []
        for arg in self.arguments:
            if (arg.name in argument_names):
                argument_names.remove(arg.name)
            else:
                to_remove.append( arg )
        for arg in to_remove: self.arguments.remove(arg)
        # Check all interfaces for declarations of missing argument names.
        from .procedure import Procedure
        empty_interfaces = []
        for i in self.interfaces:
            # Check functions.
            to_remove = []
            for f in i.functions:
                if f.name in argument_names:
                    argument_names.remove(f.name)
                    self.arguments.append( Procedure([Procedure.type], parent=self) )
                    self.arguments[-1].name = f.name
                    self.arguments[-1].kind = "C_" + f.name
                    self.arguments[-1].intent = "IN"
                    self.arguments[-1].show_intent = False
                else:
                    to_remove.append(f)
            for f in to_remove: i.functions.remove(f)
            # Check subroutines.
            to_remove = []
            for f in i.subroutines:
                if f.name in argument_names:
                    argument_names.remove(f.name)
                    self.arguments.append( Procedure([Procedure.type], parent=self) )
                    self.arguments[-1].name = f.name
                    self.arguments[-1].kind = "C_" + f.name
                    self.arguments[-1].intent = "IN"
                    self.arguments[-1].show_intent = False
                else:
                    to_remove.append(f)
            for f in to_remove: i.subroutines.remove(f)
            # Mark this interface for removal, if it is empty.
            if ((len(i.functions) + len(i.subroutines)) == 0):
                empty_interfaces.append( i )
        # If there are any remaining undefined arguments..
        if (len(argument_names) > 0):
            from fmodpy.config import implicit_typing
            # Assign implicit INTEGER and REAL types to undefined arguments.
            if (implicit_typing):
                integer_letters = {'I', 'J', 'K', 'L', 'M', 'N'}
                for a in argument_names:
                    if (a[0] in integer_letters): line = f"INTEGER {a}"
                    else:                         line = f"REAL {a}"
                    arg = parse_argument([line], "", self)
                    assert (len(arg) == 1)
                    self.arguments.append( arg[0] )
            # Default behavior is to NOT support implicit typing.
            # Rather, assume that fmodpy has incorrectly parsed the
            # procedure.
            else:
                from fmodpy.exceptions import ParseError
                raise(ParseError(f"Finished parsing {self.type} {self.name}, but never declared {', '.join(argument_names)}."))
        # Remove empty interfaces.
        for i in empty_interfaces: self.interfaces.remove(i)
        # Sort the arguments in this subroutine according to their
        # position in the declared argument list.
        self.arguments.sort(key=lambda arg: argument_order.index(arg.name))


    # Return a list of lines that defines a Fortran -> C interface for
    # this routine. It will not contain the body code.
    def fortc_interface(self):
        # Define the interface to the C-function that will be called
        # from Fortran to evaluate externally provided routines.
        lines = []
        raise(NotImplementedError)

    # Return a list of lines that defines a Fortran -> C interface
    # that appropriate converts Fortran locals to C inputs.
    def fortc_subroutine(self):
        # Add the first line defining this FUNCTION / SUBROUTINE
        # Take all the normal inputs (given in the original INTERFACE)
        # Declare all arguments and local variables.
        # Translate Fortran arguments into C information.
        # Call C function (defined through abstract interface).
        # Translate C results back into Fortran arguments.
        raise(NotImplementedError)

    # Return a list of lines that defines a C -> Python interface
    # that allows Python code to be called from Fortran (through C).
    def cpy_function(self):
        # Translate C into NumPy values.
        # Call user-provided function, collect output.
        # Translate NumPy values back into C
        raise(NotImplementedError)

    # Generate Fortran wrapper code that can call this Subroutine's source code.
    def generate_fortran(self):
        # Generate the input signature.
        fortran_arguments = []
        for arg in self.arguments: fortran_arguments += arg.fort_input()
        # Regardless of if this is a FUNCTION or SUBROUTINE, we will always
        #  wrap with a SUBROUTINE to avoid RESULT related name conflicts.
        lines = ['',f"SUBROUTINE C_{self.name}({', '.join(fortran_arguments)}) BIND(C)"]
        # Add the "USE" line.
        lines += ["  "+l for l in self.uses]
        if any((a.allocatable and a._is_output()) for a in self.arguments):
            lines += ["  USE ISO_FORTRAN_ENV, ONLY: INT64"]
        # Check if this is in a module.
        in_module = (self.parent is not None) and (self.parent.type == "MODULE")
        if (in_module): lines += [f"  USE {self.parent.name}, ONLY: {self.name}"]
        # Enforce no implicit typing (within this code).
        lines += [f"  IMPLICIT NONE"]
        # Add all argument declarations.
        for arg in self.arguments:
            lines += ["  " + l for l in arg.fort_declare()]
        lines += ['']
        # If this is not inside of a module, need to define an interface.
        if ((not in_module) or (len(self.interfaces) > 0)):
            # Add the "INTERFACE" line for the source subroutine.
            lines += ["  INTERFACE"]
            # Need to define the interface to the C function that
            #   will be called by the internal Fortran wrapper.
            for i in self.interfaces:
                for f in i.functions:   lines += ["    "+l for l in f.fortc_interface()]
                for f in i.subroutines: lines += ["    "+l for l in f.fortc_interface()]
            # Need to define the interface to the actual function
            # being called (if this is not part of a module).
            if (not in_module):
                lines += ["    "+l for l in str(self).split("\n")]
            lines += ["  END INTERFACE",'']
        # Add all argument preparation code.
        for arg in self.arguments:
            lines += ['  '+l for l in arg.fort_prepare()]
        # Add the call line.
        optional_args = [a for a in self.arguments if a.optional]
        mandatory_args = [a for a in self.arguments if not a.optional]
        im_a_function = hasattr(self, "result")
        # Set the beginning of the line that calls the source routine.
        if im_a_function:
            for arg in mandatory_args:
                if (im_a_function and (arg.name == self.result)):
                    call_start = f"{arg.fort_call_name()} ="
                    mandatory_args.remove(arg)
                    break
        else: call_start = "CALL"
        # Generate the calling code recursively when there are optionals.
        if (len(optional_args) > 0):
            # If there are any optionals, assign all arguments
            # literally in the call to Fortran (because the ordering
            # could be off after accounting for presence).
            always_assigned = [f"{a.name}={a.fort_call_name()}" for a in mandatory_args]
            # Define a recursive routine to write out all cases for the presence
            # of the optional variables using nested 'IF PRESENT()' conditions.
            def make_call_with_optionals(present, missing, optionals,
                                         indent="  ", lines=lines):
                if (len(optionals) > 0):
                    a = optionals[0]
                    lines += [f"{indent}IF ({a.fort_present_name()}) THEN"]
                    make_call_with_optionals(present+[a], missing, optionals[1:], indent+"  ")
                    lines += [f"{indent}ELSE"]
                    make_call_with_optionals(present, missing+[a], optionals[1:], indent+"  ")
                    lines += [f"{indent}END IF"]
                else:
                    # Get all assigned names.
                    assigned = [f"{a.name}={a.fort_call_name()}" for a in present]
                    call_args = ", ".join(always_assigned+assigned)
                    # Add the call line.
                    lines += [f"{indent}{call_start} {self.name}({call_args})"]
                    # Produce lines that come after the call that are
                    # specific to the presence of the optional arguments.
                    for a in present: lines += [indent+l for l in a.fort_after(present=True)]
                    for a in missing: lines += [indent+l for l in a.fort_after(present=False)]
            # Use the recursive call-generating function.
            make_call_with_optionals([], [], optional_args)
        else:
            call_args = ', '.join([a.fort_call_name() for a in mandatory_args])
            lines += [f"  {call_start} {self.name}({call_args})"]

        # Put the "RESULT" back into the list of mandatory arguments.
        if im_a_function:
            mandatory_args += [a for a in self.arguments if (a.name == self.result)]

        # Add all the argument post-processing code (for mandatory arguments).
        for arg in mandatory_args:
            fort_after = arg.fort_after()
            if (len(fort_after) > 0): fort_after = ['']+fort_after
            lines += ['  '+l for l in fort_after]

        # Add a declaration of all abstract interfaces (if there are any)
        # for calling back into C from inside of Fortran.
        if (len(self.interfaces) > 0):
            lines += ['', 'CONTAINS']
            for i in self.interfaces:
                # TODO: Need to define the interface to the C function that
                #       will be called by the internal Fortran wrapper.
                for f in i.functions: lines += ["  "+l for l in f.fortc_subroutine()]
                for f in i.subroutines: lines += ["  "+l for l in f.fortc_subroutine()]
            lines += ['']

        # Add the END line.
        lines += [f"END SUBROUTINE C_{self.name}",'']
        return lines

    # Generate Python code that accesses this Subroutine.
    def generate_python(self, type_blocks=None):
        from fmodpy.config import fmodpy_print as print
        lines = [ '',
                  "# ----------------------------------------------",
                 f"# Wrapper for the Fortran subroutine {self.name}",
                  '']
        py_name = self.name.lower()
        # Add the Python-callable function.
        py_input = []
        # If this is in a module, then the first attribute will be "self".
        in_module = (self.parent is not None) and (self.parent.type == "MODULE")
        if in_module: py_input.append("self")
        # Cycle args (make sure the ones that are optional are listed last).
        all_types = set()
        for arg in sorted(self.arguments, key=lambda a: int(a._is_optional())):
            py_input += arg.py_input()
            # Add the python type declaration blocks if appropriate.
            if (arg.py_type is not None):
                all_types.add(arg.py_type)
        # Declare the function and add the documentation.
        lines += [f"def {py_name}({', '.join(py_input)}):",
                  f"    '''{self.docs}'''"]
        # If there is no parent to capture the types, make them in the subroutine.
        if (type_blocks is None):
            # Add lines for the types.
            for tb in sorted(all_types):
                lines += [""]
                lines += ["    "+l for l in tb.split("\n")]
        # Otherwise, add the type blocks to the parent set to be processed.
        else:
            for tb in all_types:
                type_blocks.add(tb)
        # Add the declaration lines.
        py_declare = []
        for arg in self.arguments:
            arg_declare = arg.py_declare()
            if (len(arg_declare) > 0):
                py_declare += ['',f'# Setting up "{arg.name.lower()}"'] + arg_declare
        lines += ['    '+l for l in py_declare]
        # Add call line.
        py_call = []
        for arg in self.arguments: py_call += arg.py_call()
        lines += ['',"    # Call C-accessible Fortran wrapper.",
                    f"    clib.c_{py_name}({', '.join(py_call)})", '']
        # Add post-processing line.
        py_after = []
        for arg in self.arguments:
            arg_after = arg.py_after()
            if (len(arg_after) > 0):
                arg_after = [f'# Post-processing "{arg.name.lower()}"']+ arg_after +['']
            py_after += arg_after
        lines += ["    "+l for l in py_after]
        # Add return line.
        py_return = []
        for arg in self.arguments: py_return += arg.py_return()
        lines += [ "    # Return final results, 'INTENT(OUT)' arguments only.",
                  f"    return {', '.join(py_return)}"]
        return lines

