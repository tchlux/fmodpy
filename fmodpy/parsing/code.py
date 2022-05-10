from . import class_name

# Base class for all blocks of Fortran code that will be wrapped. This
# information describes the code in a common format.
class Code:
    # Description
    name = ""
    type = None
    docs = ""
    lines = 0
    allowed_unknown = False
    # Does the end of this Code have to be named?
    needs_end = True
    named_end = True
    # Parent module.
    parent = None
    # Declare some naming convention for parsing
    prefixes = []
    # List of (func, str) pairs where the string is the local attribute
    # name and the function is a parser returning a list of objects.
    can_contain = []
    # List of (func) parsing functions that handle ignorable contents.
    will_ignore = []

    # Initialize this object.
    def __init__(self, list_of_lines, preceding_comments="", parent=None):
        self.parent = parent
        self.docs = preceding_comments
        # Initizlie lists of contained objects.
        for (_, name) in self.can_contain: setattr(self, name, [])
        # Parse the rest of this abstract object from file.
        # Count how many lines this object has by tracking change.
        starting_length = len(list_of_lines)
        self.parse(list_of_lines)
        self.lines += (starting_length - len(list_of_lines))

    # Given a list of strings (lines of simplified Fortran file),
    # where then entire string has been converted to upper case,
    # initialize this object.
    def parse(self, list_of_lines):
        from fmodpy.config import fmodpy_print as print
        print(f"{self.type.title()}.parse {self.name}")
        # Extract documentation first.
        while (len(list_of_lines) > 0):
            stripped_line = list_of_lines[0].strip() + '!'
            if (stripped_line[0] == '!'):
                self.docs += "\n" + list_of_lines.pop(0)
            # If this is not a comment line, the docs are done.
            else: break
        # Strip off any whitespace from the edges of the docs.
        self.docs = self.docs.strip()
        # Strip off all empty comment lines from the end of the comment block.
        while (self.docs[-2:] == "\n!"): self.docs = self.docs[:-2]
        # Parse the rest of the file (popping out lines from list).
        ended = False
        comments = ""
        while (len(list_of_lines) > 0):
            line = list_of_lines[0].strip().split()
            if len(line) == 0:
                comments = "" # empty new lines before something indicate no comment connection
                list_of_lines.pop(0)
                continue
            # Check if this is a comment (line should not be empty).
            elif (line[0][:1] == "!"):
                comments += "\n" + list_of_lines.pop(0)
                continue
            # Check to see if this is the END of this object.
            elif (line[0] == "END"):
                # If this line only contains the keyword "END" ...
                if (len(line) <= 1):
                    from fmodpy.config import end_is_named
                    if end_is_named:
                        from fmodpy.exceptions import ParseError
                        raise(ParseError("Encountered unexpected 'END' without block type (e.g. SUBROUTINE, MODULE)."))
                    else:
                        list_of_lines.pop(0)
                        ended = True
                        break
                # Otherwise, check what is ending ...
                elif (line[1] == self.type):
                    if (self.named_end):
                        if (len(line) < 3):
                            from fmodpy.exceptions import ParseError
                            raise(ParseError(f"Encountered unexpected '{list_of_lines[0].strip()}' without named ending."))
                        elif (line[2] == self.name):
                            list_of_lines.pop(0)
                            ended = True
                            break
                    # This does not require a named ending, we are done.
                    else:
                        list_of_lines.pop(0)
                        ended = True
                        break
                # This is the end of something else (not me), ignore it.
                else:
                    list_of_lines.pop(0)
                    continue
            # Parse the contained objects out of the file.
            for (parser, name) in self.can_contain:
                pre_length = len(list_of_lines)
                instances = parser(list_of_lines, comments, self)
                length = pre_length - len(list_of_lines)
                self.lines += length
                # If instances were found, we have a match, break.
                if (len(instances) > 0):
                    for inst in instances:
                        getattr(self,name).append( inst )
                    comments = ""
                    break
                # If any parsing was done otherwise, then it was a match, break.
                elif (length > 0): break
            else:
                # Look for things that will be parsed, but ignored.
                for parser in self.will_ignore:
                    pre_length = len(list_of_lines)
                    instances = parser(list_of_lines, comments, self)
                    length = len(list_of_lines) - pre_length
                    self.lines += length
                    if ((len(instances) > 0) or (length > 0)): break
                else:
                    # This is an unknown block of code.
                    if self.allowed_unknown:
                        comments = ""
                        # This is an un-identifiable line, it belongs ???
                        print(f"  skipping line '{list_of_lines.pop(0)}'")
                    else:
                        from . import class_name
                        from fmodpy.exceptions import ParseError
                        raise(ParseError(f"\n\nEncountered unrecognized line while parsing {class_name(self)}:\n\n  {str([list_of_lines.pop(0)])[1:-1]}\n\n"))
        # Check for correct ending.
        if (self.needs_end and (not ended)):
            from fmodpy.exceptions import ParseError
            raise(ParseError(f"File ended without observing 'END {self.type}'."))
        # Finalize docs (knowing they will be formatted soon).
        self.docs = self.docs.replace("{","{{").replace("}","}}")
        # Denote the end of the docs.
        print(f" {self.type.title()}.parse done.")


    # Parse the contained objects into Fortran BIND(C) code.
    def generate_fortran(self):
        lines = []
        for (_, name) in self.can_contain:
            for instance in getattr(self, name):
                lines += instance.generate_fortran()
        return lines

    # Parse the contained objects into python code lines, making
    # Python-callable code for all contained objects in this Code.
    def generate_python(self):
        lines = []
        type_blocks = set()
        for (_, name) in self.can_contain:
            for instance in getattr(self, name):
                # TODO: Find a way to generalize this pattern better,
                #       should not need custom logic like this.
                #       Right now it collects types from all children
                #       then adds those type definitions to self.
                args = []
                if (instance.type in {"SUBROUTINE","FUNCTION","MODULE"}):
                    args.append(type_blocks)
                # Get the lines of python code.
                lines += instance.generate_python(*args) + ['']
        # Add the type declarations if there are any.
        for block in sorted(type_blocks):
            lines.insert(0,"")
            for line in reversed(block.split("\n")):
                lines.insert(0, line)
        # Return the full list of lines.
        return lines

    # Evaluate the btye-width of all contents (by compiling Fortran test).
    def eval_sizes(self, build_dir):
        # Recursively evaluate the sizes of all children.
        for (_, name) in self.can_contain:
            for instance in getattr(self, name):
                if (hasattr(instance, "eval_sizes")):
                    instance.eval_sizes(build_dir)
        # After evaluating the sizes for all children, if this code object
        # has arguments inside of it, evaluate their sizes.
        if hasattr(self, "arguments"):
            # Evaluate the size of all arguments for this routine.
            size_prog =  "PROGRAM GET_SIZE\n"
            # Add a use line for the module this is in (if it is inside one).
            if (self.parent is not None):
                # If the parent is a module, assume access to it with a USE.
                if (self.parent.type == "MODULE"):
                    size_prog += f"  USE {self.parent.name}\n"
                # Add any used modules by the parent (because this has access to those).
                if (hasattr(self.parent, "uses")):
                    for line in self.parent.uses: size_prog += f"  {line}\n"

            # # If this is a TYPE, then add the type declaration.
            # if (self.type == "TYPE"):
            #     size_prog += "\n".join(["  " + l for l in str(self).split("\n")]) + "\n"

            # If this is not a TYPE, then it might have USES statements.
            if hasattr(self, "uses"):
                for line in sorted(self.uses): size_prog += line+"\n"
            # Get the unique argument type:kind pairs, all arguments as values.
            unique_types_and_kinds = {}
            for arg in self.arguments:
                # Skip "TYPE" arguments, their sizes are evaluated separately.
                if (arg.type == "TYPE"): continue
                # Append a unique identifier to "RESULT" for functions.
                if (hasattr(self, "result") and (arg.name == self.name)):
                    arg.name += "_result"
                key = (arg.type,arg.kind)
                unique_types_and_kinds[key] = unique_types_and_kinds.get(key,[]) + [arg]
            # Add all unique (type,kind) arguments to the size check program.
            for (t,k) in sorted(unique_types_and_kinds):
                # Create a singleton argument of the same type to measure size.
                arg = unique_types_and_kinds[(t,k)][0]
                temp_arg = type(arg)([arg.type])
                temp_arg.name = arg.function_safe_name()
                temp_arg.kind = arg.kind
                temp_arg.show_intent = False
                size_prog += f"  {temp_arg}\n"
            # Add print statements (printing the sizes).
            for k in sorted(unique_types_and_kinds):
                name = unique_types_and_kinds[k][0].name
                size_prog += f"  WRITE (*,*) SIZEOF({name})\n"
            # End the program file.
            size_prog += "END PROGRAM GET_SIZE\n"
            # Take the size program string and make sure all lines 
            #  fit within the expected character width.
            from fmodpy.parsing.util import wrap_long_lines
            size_prog = "\n".join(wrap_long_lines(size_prog.split("\n")))
            # Import some necessary configurations to make the SIZEOF program.
            from fmodpy.config import run, f_compiler, \
                GET_SIZE_PROG_FILE, GET_SIZE_EXEC_FILE
            import os
            size_prog_path = os.path.join(build_dir, GET_SIZE_PROG_FILE)
            size_exec_path = os.path.join(build_dir, GET_SIZE_EXEC_FILE)
            # Write the size program.
            with open(size_prog_path, "w") as f: f.write(size_prog)
            # Compile an executable "GET_SIZE" program. All of the 
            # necessaray modules (for typing) should already be compiled.
            code, stdout, stderr = run([f_compiler, "-o", size_exec_path, size_prog_path])
            if (code != 0):
                from fmodpy.exceptions import CompileError
                error_message = "\n\nCompilation of a program for size checking failed.\n"+\
                                     "This may have happened because necessary module files\n"+\
                                     "are not compiled and present in the build directory.\n\n"+\
                                "\n".join(stderr)
                raise(CompileError(error_message))
            # Run the "GET_SIZE" executable.
            code, stdout, stderr = run([size_exec_path])
            if (code != 0): raise(NotImplementedError("Argument byte-size checking program failed, unexpected."))
            # Remove the size program.
            os.remove(size_prog_path)
            os.remove(size_exec_path)
            # Read the sizes from the output.
            sizes = [s.strip() for s in stdout if s.strip().isnumeric()]
            # Assign the sizes to the appropriate arguments.
            for s, key in zip(sizes, sorted(unique_types_and_kinds)):
                for arg in unique_types_and_kinds[key]:
                    arg.size = s
