# Add spaces to syntax in lines (for easier splitting by white-space)
FORT_TEXT_REPLACEMENTS = {
    "DOUBLE PRECISION": "REAL ( KIND ( 0.0D0 ) )",
    "END DO": "ENDDO",
    "END IF": "ENDIF",
    ",":" , ",
    "(":" ( ",
    ")":" ) ",
    ":":" : ",
    "+":" + ",
    "-":" - ",
    "*":" * ",
    "/":" / ",
    "=":" = ",
}

# Some spaces that were created above were misplaced, so this fixes them.
FORT_TEXT_FIXES = {
    "= >":"=>",
}

# Keep all comment lines and all lines from a file that start with the
# following expressions (the file is converted to all upper case).
ACCEPTABLE_LINE_STARTS = {'ABSTRACT', 'CHARACTER', 'END', # 'EXTERNAL',
                          'INTEGER', 'LOGICAL', 'REAL', 'COMPLEX',
                          'IMPLICIT', 'INTERFACE', 'MODULE', 'FUNCTION',
                          'OPTIONAL', 'PRIVATE', 'PROCEDURE',
                          'PUBLIC', 'PURE', 'RECURSIVE',
                          'SUBROUTINE', 'TYPE', 'USE'}
LINE_STARTS_TO_REMOVE = {'PURE', 'RECURSIVE'}

# Immediately exclude a file from automatic compilation if it has a
# line starting with the following.
IMMEDIATELY_EXCLUDE = {"PROGRAM"}

please_report_to = f"\nIf this is syntactically correct Fortran, please report this\n"+\
                   f"as an issue with the relevant Fortran code at\n\n"+\
                   f"  https://github.com/tchlux/fmodpy\n"


from .util import after_dot, before_dot, class_name, \
    legal_module_name, simplify_fortran_file, pop_group, \
    wrap_long_lines

# --------------------------------------------------------------------

# Define a function for parsing an Interface. Returns a list of instances
def parse_interface(list_of_lines, comments, parent):
    from .interface import Interface
    return parse_code(Interface, list_of_lines, comments, parent)

# Define a function for parsing a Module. Returns a list of instances
def parse_module(list_of_lines, comments, parent):
    from .module import Module
    return parse_code(Module, list_of_lines, comments, parent)

# Define a function for parsing a Subroutine. Returns a list of instances
def parse_subroutine(list_of_lines, comments, parent):
    from .subroutine import Subroutine
    return parse_code(Subroutine, list_of_lines, comments, parent)

# Define a function for parsing a Function. Returns a list of instances
def parse_function(list_of_lines, comments, parent):
    from .function import Function
    return parse_code(Function, list_of_lines, comments, parent)

# Define a function for parsing a Type. Returns a list of instances
def parse_type(list_of_lines, comments, parent):
    from .type import TypeDeclaration
    return parse_code(TypeDeclaration, list_of_lines, comments, parent)

# Parse a PUBLIC line, identifying routines that are public.
def parse_public(list_of_lines, comments, parent, keyword="PUBLIC"):
    # Skip if there are no lines to process.
    if (len(list_of_lines) == 0): return []
    line = list_of_lines[0].strip().split()
    # Remove any comments on the line that may exist.
    if ("!" in line): line = line[:line.index("!")]
    # Skip empty lines.
    if (len(line) == 0):
        list_of_lines.pop(0)
        return []
    # Check to see if the first element is 'PUBLIC'.
    if (line[0] == keyword):
        # If there is only the word 'PUBLIC', then it is a status.
        if (len(line) == 1):
            parent.status = keyword
            # Remove this line from the list (because it has been parsed).
            list_of_lines.pop(0)
            return []
        # Strip out the double colon if it exists (it's optional).
        if (''.join(line[1:3]) == "::"): line = line[3:]
        # Remove all commas from the line.
        commas = [i for i in range(len(line)) if (line[i] == ",")]
        for i in reversed(commas): line.pop(i)
        # Remove this line from the list (because it has been parsed).
        list_of_lines.pop(0)
        # Only variable and function names remain, they are the instances.
        return line
    # Otherwise, no 'PUBLIC' line found, return empty list.
    return []

# Parse a PRIVATE line, identifying routines that are private.
def parse_private(list_of_lines, comments, parent):
    return parse_public(list_of_lines, comments, parent, keyword="PRIVATE")

# Parse a USE line, return a string in a list.
def parse_use(list_of_lines, comments, parent):
    # Skip if there are no lines to process.
    if (len(list_of_lines) == 0): return []
    line = list_of_lines[0].strip().split()
    # Skip empty lines.
    if (len(line) == 0): return []
    # Check to see if the first element is "USE".
    # If it is, then return a list with one string (the line).
    if (line[0] == "USE"): return [list_of_lines.pop(0).replace(": :","::")]
    # Otherwise, no USE line found, return empty list.
    else: return []

# Parse a line with IMPLICIT NONE, return string in a list.
def parse_implicit(list_of_lines, comments, parent):
    # Skip if there are no lines to process.
    if (len(list_of_lines) == 0): return []
    line = list_of_lines[0].strip().split()
    # Skip empty lines.
    if (len(line) == 0): return []
    # Check to see if the first element is "IMPLICIT".
    # If it is, then return a list with one string (the line).
    if (line[0] == "IMPLICIT"):
        if (len(line) > 1):
            if (line[1] == "NONE"): return [list_of_lines.pop(0)]
        else:
            from fmodpy.exceptions import ParseError
            raise(ParseError("Found phrase 'IMPLICIT' but it was not followed by 'NONE'."))
    # Otherwise, no IMPLICIT line found, return empty list.
    else: return []

# Only initialize a "code" object if the line is not empty and the
# type is matched (after removing any prefixes).
def parse_code(code, list_of_lines, comments, parent):
    # Skip if there are no lines to process.
    if (len(list_of_lines) == 0): return []
    line = list_of_lines[0].strip().split()
    # Skip empty lines.
    if (len(line) == 0): return []
    # Remove a prefix if it is at the front of the line.
    for p in code.prefixes:
        if (line[0] == p):
            line = line[1:]
            break
    # Check for an empty line (this shouldn't happen).
    if (len(line) == 0):
        import warnings
        text = f"\nAn enexpected thing just happened when parsing.\n"+\
               f"After removing {class_name(code)} prefix '{p}', the line was empty.\n"
        warnings.warn(text+please_report_to)
    # Check for a match, if it matches complete instance initialization.
    elif (line[0] == code.type):
        parsed_code = code(list_of_lines, comments, parent)
        if (parsed_code.lines > 0): return [parsed_code]
        else:                       return []
    # No objects were found, return empty list of instances.
    return []

# Given a list of strings that represent lines of a file, determine if
# it is a recognizable declaration. If so, define Argument object(s),
# return the list of Argument(s).
def parse_argument(list_of_lines, comments, parent):
    line = list_of_lines[0].strip().split()
    success = False
    if (len(line) == 0): return []
    # Try out all possible declarations.
    from .real import Real
    from .integer import Integer
    from .logical import Logical
    from .character import Character
    from .complex import Complex
    from .type import TypeArgument
    for arg_type in [Real, Integer, Logical, Character, Complex, TypeArgument]:
        if (line[0] == arg_type.type):
            success = True
            break
    else: return []
    # If an argument type was identified, then finish parsing.
    double_colon = [i for i in range(len(line)-1)
                    if line[i] == line[i+1] == ":"]
    # If there is no "::", then variable names will immediately follow type(kind).
    # TODO: This might be a parsing error, or a FortranLanguageError.
    if (len(double_colon) > 1): raise(NotImplementedError)
    elif (len(double_colon) == 1):
        # There is a double colon in this line.
        double_colon = double_colon[0]
        base = line[:double_colon]
        tail = line[double_colon+2:]
    else:
        # If there is a KIND, then include that in the base.
        if ((len(line) > 2) and (line[1]) == '('):
            # Check to see of there is a paranthetical group after
            #   the arugment type (this would be for a KIND).
            kind, tail = pop_group(line[1:], open_with="(", close_with=")")
            base = [line[0]] + ["("] + kind + [")"]
        else:
            base = line[:1]
            tail = line[1:]
    # Check to make sure there are variable names after the TYPE.
    if (len(tail) == 0): raise(NotImplementedError)        
    # Now we are guaranteed to use this line to define an argument.
    list_of_lines.pop(0)
    # Get all names from the tail (with their dimensions, if given).
    names = [tail.pop(0)]
    while (len(tail) > 0):
        # First, check to see of there is a paranthetical group after
        #   the previous argument name (this would be for a size, like
        #   ARG(10,2), but it could have expresssions inside as well).
        group, tail = pop_group(tail, open_with="(", close_with=")")
        if (len(group) > 0): names[-1] += " ( "+" ".join(group)+" )"
        # If there is a "," it indicates more arguments follow.
        elif (tail[0] == ","): tail.pop(0)
        # If this is a PARAMETER assignment, there could be an "= value"
        #  after the name. There will only be other arguments after
        #  this one if there is a "," in the line. If no "," then stop.
        elif (tail[0] == "="):
            if (("," in tail) and ("=" in tail[1:])):
                tail = tail[tail.index(",")+1:]
            else: break
        # Finally, if it is not a comma, group, or value, it must be an argument name.
        else: names.append(tail.pop(0))
    # Cycle the names, converting all into proper Argument objects.
    args = []
    for n in names:
        args.append( arg_type(base[:]) )
        args[-1].parent = parent
        # Split the dimension out of the name, if it is there.
        if "(" in n:
            begin_dim = n.index("(")
            n, group = n[:begin_dim].strip(), n[begin_dim:].strip().split()
            group, _ = pop_group(group)
            dimension = [""]
            num_open = 0
            while (len(group) > 0):
                next_value = group.pop(0)
                if (next_value == ",") and (num_open == 0):
                    dimension.append("")
                else: dimension[-1] += next_value
                if (next_value == "("): num_open += 1
                if (next_value == ")"): num_open -= 1
            # Overwrite the dimension.
            args[-1].dimension = dimension
        # Overwrite the name.
        args[-1].name = n
    # Return the final list of declared arguments, and success.
    return args
