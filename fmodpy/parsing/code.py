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

    # Generate a string describing this code.
    def __str__(self):
        out = f"{self.type} {self.name}"
        for (_, name) in self.can_contain:
            if (len(getattr(self, name)) > 0): out += "\n"
            for obj in getattr(self,name):
                for line in str(obj).split("\n"):
                    out += "  "+line+"\n"
        return out


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
        while (len(list_of_lines) > 0) and (list_of_lines[0].strip()[:1] == "!"):
            self.docs += "\n" + list_of_lines.pop(0)
        self.docs = self.docs.strip()
        # Parse the rest of the file (popping out lines from list).
        ended = False
        comments = ""
        while (len(list_of_lines) > 0):
            line = list_of_lines[0].strip().split()
            if len(line) == 0:
                comments = ""
                list_of_lines.pop(0)
                continue
            # Check if this is a comment (line should not be empty).
            if (line[0][:1] == "!"):
                comments += "\n" + list_of_lines.pop(0)
                continue
            # Check to see if this is the END of this object.
            if (line[0] == "END"):
                if (len(line) < 2):
                    from fmodpy.exceptions import ParseError
                    raise(ParseError("Encountered unexpected 'END' without block type (e.g. SUBROUTINE, MODULE)."))
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
                length = len(list_of_lines) - pre_length
                self.lines += length
                if (len(instances) > 0):
                    print(f"  parsed {len(instances)} into '{name}'..")
                    for inst in instances:
                        getattr(self,name).append( inst )
                    comments = ""
                    break
            else:
                # Look for things that will be parsed, but ignored.
                for parser in self.will_ignore:
                    pre_length = len(list_of_lines)
                    instances = parser(list_of_lines, comments, self)
                    length = len(list_of_lines) - pre_length
                    self.lines += length
                    if (len(instances) > 0): break
                else:
                    # This is an unknown block of code.
                    if self.allowed_unknown:
                        comments = ""
                        # This is an un-identifiable line, it belongs ???
                        print(f"  skipping line '{list_of_lines.pop(0)}'")
                    else:
                        from . import class_name
                        from fmodpy.exceptions import ParseError
                        raise(ParseError(f"\n\nEncountered unrecognized line while parsing {class_name(self)}:\n\n{list_of_lines.pop(0)}\n\n"))
        # Check for correct ending.
        if (self.needs_end and (not ended)):
            from fmodpy.exceptions import ParseError
            raise(ParseError(f"File ended without observing 'END {self.type}'."))
        print(f" {self.type.title()}.parse done.")


    # Parse the contained objects into Fortran BIND(C) code.
    def generate_fortran(self):
        lines = []
        for (_, name) in self.can_contain:
            for instance in getattr(self, name):
                lines += instance.generate_fortran()
        return lines

    # Parse the contained objects into cython code lines, making
    # Python-callable code for all contained objects in this Code.
    def generate_cython(self):
        lines = []
        for (_, name) in self.can_contain:
            for instance in getattr(self, name):
                lines += instance.generate_cython() + ['']
        # Return the full list of lines.
        return lines

    # Evaluate the btye-width of all contents (might compiling Fortran).
    def eval_sizes(self, build_dir):
        for (_, name) in self.can_contain:
            for instance in getattr(self, name):
                instance.eval_sizes(build_dir)
