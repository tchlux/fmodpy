from fmodpy.parsing import FORT_TEXT_REPLACEMENTS, FORT_TEXT_FIXES

# Short functions.
#   identifying the strings before and after the last "."
def before_dot(name): return name[:len(name) - 1 - name[::-1].find(".")];
def after_dot(name): return name[min(-name[::-1].find("."),0):]
#   getting the class name of something, stripping module prefix
def class_name(cls): return str(type(cls)).split(".")[-1].split("'")[0]
#   identifying legal python module names
def legal_module_name(name):
    return (name.replace("_","")[0].isalpha() and
            name.replace("_","")[1:].isalnum() and
            (len(name) > 0))

# Shorten all strings to be 132 characters at maximum given a list of str.
def wrap_long_lines(list_of_lines, max_len=132):
    i = -1
    while (i+1 < len(list_of_lines)):
        i += 1
        # Get the line (strip off any comments for length checks).
        line = list_of_lines[i]
        if "!" in line: line = line[:line.index("!")]
        # Check the length of the (uncommented) line.
        if (len(line) > max_len-1):
            # Break up this line if it is too long.
            keep, rest = list_of_lines[i][:max_len-1], list_of_lines[i][max_len-1:]
            list_of_lines[i] = keep+"&"
            list_of_lines.insert(i+1, "&"+rest)
    return list_of_lines

# Function for efficiently performing a series of replacements on a line of text
def clean_text(text, replacements=FORT_TEXT_REPLACEMENTS, fixes=FORT_TEXT_FIXES):
    import re
    # Create a proper reg-exp dictionary of replacements
    replacements = {re.escape(k):v for (k,v) in replacements.items()}
    # Generate a regular expression pattern with that dictionary
    pattern = re.compile("|".join(replacements.keys()))
    # Perform the replacement using python reg-exp search
    cleaned_file = pattern.sub(lambda m: replacements[re.escape(m.group(0))], text)
    # Now repeat the above steps undoing any broken elements.
    fixes = {re.escape(k):v for (k,v) in fixes.items()}
    pattern = re.compile("|".join(fixes.keys()))
    fixed_file = pattern.sub(lambda m: fixes[re.escape(m.group(0))], cleaned_file)
    return fixed_file

# Read a fortran file, store it as single lines 
# (without leading and trailing whitespace) and return
def simplify_fortran_file(in_file, old_fortran=False):
    from fmodpy.parsing import ACCEPTABLE_LINE_STARTS, \
        LINE_STARTS_TO_REMOVE, IMMEDIATELY_EXCLUDE
        
    with open(in_file) as f:
        fort_file = []
        curr_line = ""
        for line in f.readlines():
            if (old_fortran) and (len(line) >0) and (line[0].upper() == "C"):
                line = "!" + line[1:]
            # Split out the comments from the line
            comment_start = line.find("!") if (line.find("!") != -1) else len(line)
            line, comment = line[:comment_start], line[comment_start:].strip()
            # Keep lines that are strictly comments (might be documentation)
            if len(line.strip()) == 0:
                fort_file.append(comment)
                continue
            # Make all fortran upper case
            line = line.upper()
            # Break the line by the colon character
            lines = line.split(";")
            for line in lines:
                if len(line.strip()) == 0: continue
                if (old_fortran):
                    line = line[:72]
                    # Line continuation character in column 5
                    if (len(line) > 5) and (line[5] in ["1","*"]):
                        line = "&" + line[6:]
                        # Retro-actively pop the previous line for continuation
                        if len(curr_line) == 0: curr_line = fort_file.pop(-1)
                    # Remove any numeric labels from the front of the line
                    line = line.strip().split()
                    if (len(line) > 0) and (line[0].isnumeric()): line = line[1:]
                    line = " ".join(line)
                # After processing old fortran, properly strip the line of whitespace
                line = line.strip()
                # Remove a leading ampersand if necessary
                if (line[0] == "&"):
                    line = line[1:]
                curr_line += line
                if ((len(line) > 0) and (line[-1] == "&")):
                    curr_line = curr_line[:-1]
                else:
                    # Process the line into a common format
                    #   (upper case, space separated, no commas)
                    clean_line = clean_text(curr_line)
                    line = [v.strip() for v in clean_line.split()]
                    # Only take lines that are acceptable
                    acceptable = (len(line) > 0) and (
                        ("!" in line[0]) or (line[0] in ACCEPTABLE_LINE_STARTS))
                    # Check for certain exclusions (like "PROGRAM").
                    if (len(line) > 0) and (line[0] in IMMEDIATELY_EXCLUDE):
                        from fmodpy.exceptions import FortranError
                        raise(FortranError(("A valid fortran python module cannot"+
                                            " contain '%s'.")%(line[0])))
                    # Store the line if it was acceptable.
                    if acceptable:
                        # Remove line starts that can be ignored safely.
                        while ((len(line) > 0) and (line[0] in LINE_STARTS_TO_REMOVE)):
                            line.pop(0)
                        # Add the line to the simplified Fortran file.
                        fort_file.append( " ".join(line) )
                    curr_line = ""
    return fort_file


# Given a list of strings, return the group of elements between
# <open_with> and <close_width>. This is initially designed to extract
# the elements within an open-close parenthesis while allowing for
# nested parenthetical groups. Returns two lists, one containing the
# group (if it starts at the beginning of <list_str>, else empty), the
# other containing the remainder of the list of strings.
def pop_group(list_str, open_with="(", close_with=")"):
    group = []
    # Get the first element of the string (open the group, if matched).
    if ((len(list_str) > 0) and (list_str[0] == open_with)):
        list_str.pop(0)
        num_open = 1
    else: num_open = 0
    # Search until the started group is closed.
    while (num_open > 0):
        # TOOD: Might need to raise parsing error when this happens.
        if (len(list_str) == 0): raise(NotImplementedError)
        next_value = list_str.pop(0)
        if   (next_value == open_with):  num_open += 1
        elif (next_value == close_with): num_open -= 1
        if (num_open != 0): group.append(next_value)
    # Return the captured portion and the remaining.
    return group, list_str
        
