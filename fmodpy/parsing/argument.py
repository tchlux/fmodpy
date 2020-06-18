from . import pop_group

# --------------------------------------------------------------------
# Base class for all "Argument" objects that contain information about
# the pieces of data that need to be passed between Python and Fortran.
class Argument:
    parent = None # Container of this argument (Module, Subroutine, etc.)
    type = "" # argument type (REAL, INTEGER, etc.)
    name = "" # name of this argument in code
    size = "" # size in bytes (uses compiled code to evaluate)
    kind = "" # KIND setting
    intent = "INOUT"
    show_intent = True
    allocatable = False # May be (re)allocated at runtime.
    optional = False # May not be provided as input.
    save = False # Save value between executions.
    value = False # Pass by value, not by reference.
    pointer = False # Pass by pointer, not by reference.
    dimension = None # If array, this describes the shape.
    c_types = {} # The C-types used to declare this argument (key is self.size)
    np_types = {} # The NumPy types used to declare this argument (key is self.size)
    default_singleton = "1" # The default value assigned to a singleton.

    # Names of arguments that will be given to the Fortran wrapper.
    def fort_input(self):
        names = []
        # Add a boolean "_PRESENT" for this variable if optional.
        if (self.optional):
            names.append(self.name+"_PRESENT")
        # Add extra arguments for the dimension sizes.
        if (self.dimension is not None):
            for i in range(len(self.dimension)):
                names.append(f"{self.name}_DIM_{i+1}")
        # Append the actual name of this variable.
        names.append(self.name)
        # Add an identifier to this variable if it has the name of a function.
        if (self.parent is not None) and (self.parent.name == self.name):
            names[-1] += "_RESULT"
        return names

    # Lines of Fortran code necessary to declare variables for this argument.
    def fort_declare(self):
        lines = []
        if (self.optional):
            lines.append(f"LOGICAL, INTENT(IN) :: {self.name}_PRESENT")
        # If this is an allocatable, the sizes have to be given as output.
        size_type = "IN" if not self.allocatable else "OUT"
        # Add extra arguments for the dimension sizes.
        if (self.dimension is not None):
            og_dimension = self.dimension[:]
            for i in range(len(self.dimension)):
                lines.append(f"INTEGER, INTENT({size_type}) :: {self.name}_DIM_{i+1}")
                # If this is allocatable, the dimension cannot be assumed on input.
                if self.allocatable: self.dimension[i] = ":"
                else: self.dimension[i] = f"{self.name}_DIM_{i+1}"
        # Add a local variable that will have the "ALLOCATABLE" metadata.
        if self.allocatable:
            temp_arg = self.copy()
            temp_arg.save = True
            temp_arg.name += "_LOCAL"
            temp_arg.show_intent = False
            temp_arg.optional= False
            lines.append(str(temp_arg))
        # Append actual name of this variable (with dimension
        # values filled to be the names of the integer inputs 
        # and OPTIONAL / ALLOCATABLE turned off).
        temp_arg = self.copy()
        if (self.parent is not None) and (self.parent.name == self.name):
            temp_arg.name += "_RESULT"
        temp_arg.optional = False
        temp_arg.allocatable = False
        # If this is allocatable, it will be returned as an INT64 memory address.
        if self.allocatable:
            temp_arg.dimension = None
            temp_arg.intent = "OUT"
            temp_arg.show_intent = True
            temp_arg.type = "INTEGER"
            temp_arg.kind = "INT64"
        # Add the actual definition of this argument.
        lines.append(str(temp_arg))
        # Reset the dimension, if it had one beforehand.
        if (self.dimension is not None): self.dimension = og_dimension
        # Return the lines of declaration.
        return lines

    # Return the name of this variable that should be used locally,
    # safe for use with RESULT values from FUNCTIONs that do not
    # define a separate name for results.
    # 
    # WARNING: This function returns a string not a list, because the
    #          size checking will have an exact 1-1 mapping of arguments.
    def function_safe_name(self):
        name = self.name
        if (self.parent is not None) and (self.parent.name == self.name):
            name += "_RESULT"
        return name

    # Lines of Fortran code that must be executed before the call.
    def fort_prepare(self): return []

    # Return the string name of this variable for "PRESENT" checking.
    # WARNING: This is only used by "OPTIONAL" arguments.
    def fort_present_name(self): return self.name + "_PRESENT"

    # WARNING: This function returns a string not a list, because the
    #          Fortran call will have an exact 1-1 mapping of arguments.
    def fort_call_name(self):
        name = self.name
        if (self.parent is not None) and (self.parent.name == self.name):
            name += "_RESULT"
        elif (self.allocatable):
            name += "_LOCAL"
        return name

    # Lines of Fortran code that must be executed after the call.
    def fort_after(self, present=True):
        lines = []
        is_output = ("OUT" in self.intent) or (self.intent == "")
        # Copy over the size of the output allocatable array if it was not known.
        if (is_output and self.allocatable):
            if (self.dimension is None): raise NotImplementedError
            for i in range(1,len(self.dimension)+1):
                if present: size = f"SIZE({self.function_safe_name()}_LOCAL,{i})"
                else:       size = "0"
                lines.append(f"{self.function_safe_name()}_DIM_{i} = {size}")
            # Copy the allocatable address only when it is present.
            if present:
                lines.append(f"{self.function_safe_name()} = LOC({self.name}_LOCAL(1))")
        # Return the list of lines.
        return lines

    # The names of arguments as described in a C representation, this 
    # will perfectly match the inputs of the Fortran wrapper code.
    def c_input(self):
        names = []
        if (self.optional): names += ["bint* "+self.name.lower()+"_present"]
        # Add dimension size arguments.
        if self.dimension is not None:
            for i in range(len(self.dimension)):
                names += ["int* "+self.name.lower()+f"_dim_{i+1}"]
        # Determine the C type.
        if (self.size in self.c_types):
            c_type = self.c_types[self.size]
        else:
            from fmodpy.config import fmodpy_print as print
            print(f"ERROR: unknown size '{self.size}'")
            raise(NotImplementedError)
        # Determine the handling of pointers if allocatable.
        if (not self.allocatable):
            names += [f"{self.c_types[self.size]}* {self.name.lower()}"]
        elif (not self._is_input()):
            names += [f"{self.c_types[self.size]}** {self.name.lower()}"]
        else:
            print("Currently fmodpy does not support ALLOCATABLE inputs.")
            raise(NotImplementedError)
        return names

    # The names of arguments as seen in the Python function declaration.
    def py_input(self):
        # If this is not allowed as input, do not include it.
        if (not self._allowed_input()): return []
        # This object is now either mutable or partially an input.
        elif self._is_optional(): return [self.name.lower()+"=None"]
        # Return based on size.
        if (self.size in self.c_types):
            if (self.dimension is None): dim = ""
            else: dim = f"[{','.join([':']*len(self.dimension))}]"
            return [f"{self.c_types[self.size]}{dim} {self.name.lower()}"]
        else:
            from fmodpy.config import fmodpy_print as print
            print(f"ERROR: unknown size '{self.size}'")
            raise(NotImplementedError)
        
    # The lines of Python code to execute before the Fortran call.
    def py_declare(self):
        # This is where output arrays whose dimensions are known can
        # be initialized. Everything else behaves normally.
        lines = []
        py_name = self.name.lower()
        # Get the C type.
        if (self.size in self.c_types): c_type = self.c_types[self.size]
        else:                           raise(NotImplementedError)
        # Get the NumPy type.
        if (self.size in self.np_types): np_type = self.np_types[self.size]
        else:                            raise(NotImplementedError)
        # If this is literally an optional, a boolean is required.
        if self.optional:
            lines.append(f"cdef bint {py_name}_present = True")
        # Define the dimension size variables if appropriate.
        if (self.dimension is not None):
            for i in range(len(self.dimension)):
                if (self._is_optional()): value = " = 0"
                elif (self._allowed_input()): value = f" = {py_name}.shape[{i}]"
                else: value = ""
                lines.append(f"cdef int {py_name}_dim_{i+1}{value}")
        # If inputs can be given, and they're optional, checks are needed.
        is_pointer = False
        if (self._allowed_input() and self._is_optional()):
            lines.append(f"if ({py_name} is None):")
            # If this is an optional argument, make it "not present".
            if self.optional:
                lines.append(f"    {py_name}_present = False")
            # Set the default value.
            if (self.dimension is None):
                lines.append(f"    {py_name} = {self.default_singleton}")
                lines.append(f"cdef {c_type} {py_name}_local = {py_name}")
            elif (self.allocatable):
                lines.append(f"cdef {c_type}* {py_name}_local")
                dim_def = ""
                dim_ind = ""
                if (self._is_input()):
                    # Make sure the address is of the first element in memory.
                    if (self.dimension is not None):
                        nd = len(self.dimension)                    
                        dim_def = "[" + ",".join(":"*nd) + "]"
                        dim_ind = "[" + ",".join("0"*nd) + "]"
                    lines.append(f"cdef {c_type}{dim_def} {py_name}_temp")
                    lines.append(f"if ({py_name}_present):")
                    lines.append(f"    {py_name}_temp = {py_name}")
                    lines.append(f"    {py_name}_local = &{py_name}_temp{dim_ind}")
            else:
                # Either initialize with the known size, or with size "1".
                if (self._known_size()):
                    default_shape = ', '.join(self._default_size()).lower()
                else: default_shape = ','.join('1'*len(self.dimension))
                if (self.optional):
                    not_present_shape = ','.join('1'*len(self.dimension))
                    lines.append(f"    {py_name} = numpy.zeros(shape=({not_present_shape}), dtype='{np_type}', order='F')")
                    lines.append(f"elif (type({py_name}) == bool) and ({py_name}):")
                # Create lines for initializing the default values.
                lines.append(f"    {py_name} = numpy.zeros(shape=({default_shape}), dtype='{np_type}', order='F')")
                for i in range(len(self.dimension)):
                    lines.append(f"    {py_name}_dim_{i+1} = {py_name}.shape[{i}]")
                # Check for Fortran-continuity if multi dimensional.
                lines += [f"elif (not numpy.asarray({py_name}).flags.f_contiguous):",
                          f"    raise(Exception(\"The numpy array given as argument '{py_name}' was not f_contiguous.\"))",
                          f"else:"]
                for i in range(len(self.dimension)):
                    lines.append(f"    {py_name}_dim_{i+1} = {py_name}.shape[{i}]")
                # Define the Cython memory view style interface to this object.
                lines.append(f"cdef {c_type}[{','.join([':']*len(self.dimension))}] {py_name}_local = {py_name}")
        # If this is an output-only allocatable, declare a local pointer.
        elif ((not self.optional) and self._is_output() and self.allocatable):
            lines.append(f"cdef {c_type}* {py_name}_local")
            is_pointer = True
        # Otherwise this an immutable type, declare it locally.
        elif (not self._allowed_input()):
            lines.append(f"cdef {c_type} {py_name}")
        # Return lines of declaration code.
        return lines

    # The names of arguments passed to the "BIND(C)" Fortran code.
    def py_call(self):
        names = []
        py_name = self.name.lower()
        # Add a boolean "_PRESENT" for this variable if optional.
        if (self.optional): names.append("&"+py_name+"_present")
        # Add extra arguments for the dimension sizes.
        if (self.dimension is not None):
            for i in range(len(self.dimension)):
                names.append(f"&{py_name}_dim_{i+1}")
        # Append the actual name of this variable.
        names.append("&"+py_name)
        # If this variable is optional, it has a local C declaration.
        if (self.allocatable or self._is_optional()): names[-1] += "_local"
        # If this is a memory view, then access the first element
        # (start of block of memory).
        if (self.dimension is not None) and (not self.allocatable):
            names[-1] += "["+",".join("0"*len(self.dimension))+"]"
        # Return the list of names.
        return names

    # The lines of Python code that must be executed after the Fortran call.
    def py_after(self):
        lines = []
        py_name = self.name.lower()
        if self.allocatable and self._is_output():
            # This must be an array argument and it must be 'INTENT(OUT)'.
            if (self.dimension is None): raise(NotImplementedError)
            # Get the size of the numpy variable.
            if (self.size in self.np_types):
                np_size = f"numpy.NPY_{self.np_types[self.size].upper()}"
            else: raise(NotImplementedError)
            # Get the pointer to the first index.
            local_dims = [f"{py_name}_dim_{i+1}" for i in range(len(self.dimension))]
            # Compute the size of the (flattened) array.
            lines += [f"cdef numpy.npy_intp {py_name}_size = ({') * ('.join(local_dims)})"]
            if self.optional:
                lines += [f"if ({py_name}_present):"]
                # Get flat array pointer and reshape by the dimensions
                # reversed, then transpose (f_contiguous).
                lines += [f"    {py_name} = ptr_to_numpy_array({py_name}_size, {np_size}, &{py_name}_local{'[0]'*len(self.dimension)})"]
                lines += [f"    {py_name} = {py_name}.reshape(({','.join(local_dims[::-1])})).T"]
                # Otherwise, (if not present) this output is None.
                lines += [f"else: {py_name} = None"]
            else:
                # Get flat array pointer and reshape by the dimensions
                # reversed, then transpose (f_contiguous).
                lines += [f"{py_name} = ptr_to_numpy_array({py_name}_size, {np_size}, {py_name}_local)"]
                lines += [f"{py_name} = {py_name}.reshape(({','.join(local_dims[::-1])})).T"]
        # Return all lines.
        return lines

    # The name of the argument in the final "return" line to Python.
    def py_return(self):
        names = []
        if (self._is_output()):
            # Transform array memory views back into NumPy arrays.
            is_pointer = (self._is_output() and self.allocatable)
            if (is_pointer) : name_ext = ""
            elif (self._is_optional() or self.allocatable): name_ext = "_local"
            else: name_ext = ""
            # Add this argument to the output.
            py_name = self.name.lower()
            value = f"{py_name}{name_ext}"
            # Cast to a NumPy array for return (if this is an array).
            if (self.dimension is not None): value = f"numpy.asarray({value})"
            # Return None for missing optional returns.
            if self.optional: names.append(f"({value} if {py_name}_present else None)")
            else:             names.append(f"{value}")
        # Return the processed name.
        return names

    # Return True if this variable has INTENT(IN).
    def _is_input(self): return ("IN" in self.intent)

    # Return True if this variable has INTENT(OUT).
    def _is_output(self): return ("OUT" in self.intent)

    # Check to see if this argument has known size.
    def _known_size(self): return (len(self._dimension_args()) == 0)

    # Return a list of integers that are the indices of dimensions
    # (1-indexed) that require user input definition.
    def _dimension_args(self):
        if (self.dimension is None): return []
        return [i+1 for (i,d) in enumerate(self.dimension) if d in {":","*"}]

    # Check to see if this argument is allowed as input.
    def _allowed_input(self):
        # This has "IN" in the intent.
        if self._is_input(): return True
        # This variable is strictly OUT.
        elif (self.optional): return True
        elif (self.dimension is None): return False
        elif (self.allocatable): return False
        # Could be given pre-allocated array.
        return True

    # Return True if this argument is optional (for Python users).
    def _is_optional(self):
        # This is explicitly declared optional.
        if (self.optional): return True
        # This has "IN" in the intent.
        elif (self._is_input()): return False
        # This is not allowed to be optional, because it has no input.
        elif (not self._allowed_input()): return False
        # This is an INTENT(OUT) argument.
        #   Size is known already.
        elif (self._known_size()): return True
        #   Size will be known after.
        elif (self.allocatable): return True
        #   Need to know the size.
        else: return False
        
    # Compute the default size of this Argument (assuming that
    # len(self._dimension_args()) == 0), in terms of Python, converted
    # from the assumed size of the object in Fortran.
    def _default_size(self):
        if (len(self._dimension_args()) != 0): raise(NotImplementedError)
        sizes = []
        for size in self.dimension:
            size = size.lower().replace(" ","")
            # Replace all occurrences of "SIZE(<arg>, <component>)"
            # with equivalent NumPy array syntax.
            while "size(" in size:
                start_index = size.index("size(") + len("size(")
                before_call = size[:start_index-len("size(")]
                parens = 1
                for index in range(start_index, len(size)):
                    if size[index] == "(": parens += 1
                    elif size[index] == ")": parens -= 1
                    if parens <= 0: break
                argument = size[start_index:index]
                after_call = size[index+len(")"):]
                if "," not in argument:
                    func_replacement = f"{argument}.size"
                else:
                    name = argument[:argument.index(",")]
                    dim = argument[argument.index(","):]
                    try:    dim = str(int(dim) - 1)
                    except: dim += "-1"
                    func_replacement = f"{name}.shape[{dim}]"
                # Replace float division with integer division in python.
                size = before_call + func_replacement + after_call
            # Append this NumPy size.
            sizes.append(size)
        # Return the list of sizes.
        return sizes

    # Generate a copy of this argument and return it.
    def copy(self):
        arg = type(self)([self.type])
        arg.parent = self.parent
        arg.type = self.type
        arg.name = self.name
        arg.size = self.size
        arg.kind = self.kind
        arg.intent = self.intent
        arg.show_intent = self.show_intent
        arg.allocatable = self.allocatable
        arg.optional = self.optional
        if (self.dimension is not None):
            arg.dimension = self.dimension.copy()
        return arg

    # Default initialization, process standard Fortran specifications.
    # Expects to be given list of strings that comes from a declaration
    # line, but with the ":: name1, ..." stripped off the end.
    def __init__(self, line, parent=None):
        if (len(line) == 0): raise(NotImplementedError)
        # Make sure this line matches the expected type.
        if (line.pop(0) != self.type): raise(NotImplementedError)
        # Set the parent.
        self.parent = parent
        # If the line is empty now, then we're done (use defaults).
        if (len(line) == 0): return
        # Parse the remainder of the declaration line.
        # 
        # Get the KIND declaration, if it was given.
        group, line = pop_group(line)
        if (len(group) > 0):
            if (tuple(group[:2]) == ("KIND","=")):
                group = group[2:]
            self.kind = " ".join(group)
        # The rest can come in any order, so loop over possibilities.
        while (len(line) > 0):
            # Skip commas.
            if (line[0] == ","):
                line.pop(0)
                continue
            # The following happen if the ":: names" has not been stripped.
            if (line[0] == ":"): raise(NotImplementedError)
            # Read ALLOCATABLE
            if (line[0] == "ALLOCATABLE"):
                line.pop(0)
                self.allocatable = True
                continue
            # Read DIMENSION
            if (line[0] == "DIMENSION"):
                group, line = pop_group(line[1:])
                if (len(group) == 0): raise(NotImplementedError)
                # Break up dimensions by commas, allow nested parentheses.
                self.dimension = [""]
                num_open = 0
                while (len(group) > 0):
                    next_value = group.pop(0)
                    if (next_value == ",") and (num_open == 0):
                        self.dimension.append("")
                    else: self.dimension[-1] += next_value
                    if (next_value == "("): num_open += 1
                    if (next_value == ")"): num_open -= 1
                # Done processing dimension.
                continue
            # Read INTENT
            if (line[0] == "INTENT"):
                group, line = pop_group(line[1:])
                if (len(group) == 0): raise(NotImplementedError)
                self.intent = "".join(group)
                continue
            # Read OPTIONAL
            if (line[0] == "OPTIONAL"):
                line.pop(0)
                self.optional = True
                continue            
            # Read SAVE
            if (line[0] == "SAVE"):
                line.pop(0)
                self.save = True
                continue
            # Read PARAMETER (discard this)
            if (line[0] == "PARAMETER"):
                line.pop(0)
                continue
            # Read EXTERNAL (discard this)
            if (line[0] == "EXTERNAL"): 
                line.pop(0)
                continue
            # Otherwise, this is an unrecognized argument.
            from fmodpy.config import fmodpy_print as print
            print()
            print(f"Unrecognized part of Argument '{line[0]}'.\n")
            raise(NotImplementedError)

    # Print the Fortran string declaration of this argument.
    def __str__(self):
        out = f"{self.type}"
        if (len(self.kind) > 0): out += f"(KIND={self.kind})"
        if (self.show_intent and (len(self.intent) > 0)): out += f", INTENT({self.intent})"
        if (self.optional): out += f", OPTIONAL"
        if (self.allocatable): out += f", ALLOCATABLE"
        if (self.save): out += f", SAVE"
        if ((self.dimension is not None) and
            (len(self.dimension) > 0)):
            out += f", DIMENSION({','.join(self.dimension)})"
        if (len(self.name) > 0): out += f" :: {self.name}"
        # Return the final string.
        return out
