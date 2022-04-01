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
    intent = "INOUT" # INTENT setting
    show_intent = True # if True then include INTENT in __str__
    kind_prefix = "KIND=" # String that precedes kind declaration in Fortran
    allocatable = False # May be (re)allocated at runtime.
    optional = False # May be excluded from input.
    save = False # Save value between executions.
    value = False # Pass by value, not by reference.
    pointer = False # Pass by pointer, not by reference.
    parameter = False # Whether this argument is a parameter (set at compile time)
    type_len = False # Whether this argument has the LEN parameter (in a derived TYPE)
    type_kind = False # Whether this argument has the KIND parameter (in a derived TYPE)
    dimension = None # If array, this list describes the shape and size.
    c_types = {} # The C-types used to declare this argument (key is self.size)
    c_types_arrays = {} # The C-types used to declare this argument as arrays.
    default_singleton = "1" # The default value assigned to a singleton.

    # Properties are used to add wrapper logic around retrieving types.
    @property
    def c_type(self):
        if (self.size not in self.c_types):
            raise(NotImplementedError(f"\n\nUnrecognized size '{self.size}' for argument '{self.name}', no known corresponding C type."))
        return self.c_types[self.size]
    @property
    def c_type_array(self):
        return self.c_types_arrays.get(self.size, self.c_type)
    @property
    def py_type(self): return None

    # ----------------------------------------------------------------
    #                  Generating Fortran Wrapper

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
                lines.append(f"INTEGER(KIND=SELECTED_INT_KIND(18)), INTENT({size_type}) :: {self.name}_DIM_{i+1}")
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

    # Lines of Fortran code that must be executed before the call.
    def fort_prepare(self): return []

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
                first_pos = ",".join(["1"]*len(self.dimension))
                lines.append(f"{self.function_safe_name()} = LOC({self.name}_LOCAL({first_pos}))")
        # Return the list of lines.
        return lines

    # ----------------------------------------------------------------
    #                  Generating Python Wrapper

    # The names of arguments as seen in the Python function declaration.
    def py_input(self):
        # If this is not allowed as input, do not include it.
        if (not self._allowed_input()): return []
        # This object is now either mutable or partially an input.
        elif self._is_optional(): return [self.name.lower()+"=None"]
        else: return [self.name.lower()]
        
    # The lines of Python code to execute before the Fortran call.
    def py_declare(self):
        lines = []
        # This is where output arrays whose dimensions are known can
        # be initialized. Everything else behaves normally.
        py_name = self.name.lower()
        # If this is literally an optional, a boolean is required.
        if self.optional:
            lines.append(f"{py_name}_present = ctypes.c_bool(True)")
        # If inputs can be given, and they're optional, checks are needed.
        if (self._allowed_input() and self._is_optional()):
            lines.append(f"if ({py_name} is None):")
            # If this is an optional argument, make it "not present".
            if self.optional:
                lines.append(f"    {py_name}_present = ctypes.c_bool(False)")
            # If this is a singleton, set its default value.
            if (self.dimension is None):
                lines.append(f"    {py_name} = {self.c_type}()")
                lines.append(f"else:")
                lines.append(f"    {py_name} = {self.c_type}({py_name})")
            # This has a dimension AND is allocatable.
            elif (self.allocatable):
                if (self.optional):
                    default_shape = ','.join('0'*len(self.dimension))
                    lines.append(f"    {py_name} = numpy.zeros(shape=({default_shape}), dtype={self.c_type_array}, order='F')")
                    lines.append(f"elif (type({py_name}) == bool) and ({py_name}):")
                else:
                    lines.append(f"    {py_name} = ctypes.c_void_p()")
                default_shape = ','.join('1'*len(self.dimension))
                # If the size is known, then we can initialize this optional array to pass it in.
                if (self._known_size()):
                    default_shape = ', '.join(self._default_size()).lower()
                # Create lines for initializing the default values.
                if (self._is_input() or self.optional):
                    lines.append(f"    {py_name} = numpy.zeros(shape=({default_shape}), dtype={self.c_type_array}, order='F')")
                else:
                    lines.append(f"    {py_name} = ctypes.c_void_p()")
            # This has a dimension, but is NOT allocatable.
            else:
                default_shape = ','.join('1'*len(self.dimension))
                # If this is optional (not present shape is always 1).
                if (self.optional):
                    lines.append(f"    {py_name} = numpy.zeros(shape=({default_shape}), dtype={self.c_type_array}, order='F')")
                    lines.append(f"elif (type({py_name}) == bool) and ({py_name}):")
                # If the size is known, then we can initialize this optional array to pass it in.
                if (self._known_size()):
                    default_shape = ', '.join(self._default_size()).lower()
                # Create lines for initializing the default values.
                lines.append(f"    {py_name} = numpy.zeros(shape=({default_shape}), dtype={self.c_type_array}, order='F')")
        # Convert appropriate array-inputs into Fortran compatible arrays.
        if ((self.dimension is not None) and (self._allowed_input())):
            # Check for Fortran-continuity and data type of array inputs.
            p, s = "", "" # <- prefix, spaces
            if self._is_optional(): p, s = f"el", "  "
            lines += [f"{p}if ((not issubclass(type({py_name}), numpy.ndarray)) or",
                      f"{s}    (not numpy.asarray({py_name}).flags.f_contiguous) or",
                      f"{s}    (not ({py_name}.dtype == numpy.dtype({self.c_type_array})))):",
                       "    import warnings",
                      f"    warnings.warn(\"The provided argument '{py_name}' was not an f_contiguous NumPy array of type '{self.c_type_array}' (or equivalent). Automatically converting (probably creating a full copy).\")",
                      f"    {py_name} = numpy.asarray({py_name}, dtype={self.c_type_array}, order='F')",]
        # If this is an output-only allocatable, declare a local pointer.
        elif ((not self.optional) and self._is_output() and self.allocatable):
            lines.append(f"{py_name} = ctypes.c_void_p()")
        # Otherwise this an output immutable type, declare it locally.
        elif (not self._allowed_input()):
            lines.append(f"{py_name} = {self.c_type}()")
        # This is a singleton input, convert to appropraite C type.
        elif (self._is_input() and (self.dimension is None)):
            lines.append(f"if (type({py_name}) is not {self.c_type}): {py_name} = {self.c_type}({py_name})")
        # Define the dimension size variables if appropriate.
        if (self.dimension is not None):
            # If this object is allowed as input..
            if (self._allowed_input()):
                # This is optional, so we need to check what to do with dimensions.
                if (self.optional):
                    lines.append(f"if ({py_name}_present):")
                    for i in range(len(self.dimension)):
                        lines.append(f"    {py_name}_dim_{i+1} = ctypes.c_long({py_name}.shape[{i}])")
                    lines.append("else:")
                    for i in range(len(self.dimension)):
                        lines.append(f"    {py_name}_dim_{i+1} = ctypes.c_long()")
                    if self.allocatable:
                        lines.append(f"{py_name} = ctypes.c_void_p({py_name}.ctypes.data)")
                # This is not optional, so it is declared at this point, get the dimensions.
                else:
                    for i in range(len(self.dimension)):
                        lines.append(f"{py_name}_dim_{i+1} = ctypes.c_long({py_name}.shape[{i}])")
            # This array is output, initialize dimension memory locations.
            else:
                for i in range(len(self.dimension)):
                    lines.append(f"{py_name}_dim_{i+1} = ctypes.c_long()")
        # Return lines of declaration code.
        return lines

    # The names of arguments passed to the "BIND(C)" Fortran code.
    def py_call(self):
        names = []
        py_name = self.name.lower()
        # Add a boolean "_PRESENT" for this variable if optional.
        if (self.optional): names.append("ctypes.byref("+py_name+"_present)")
        # Add extra arguments for the dimension sizes.
        if (self.dimension is not None):
            for i in range(len(self.dimension)):
                names.append(f"ctypes.byref({py_name}_dim_{i+1})")
        # Append the actual name of this variable.
        names.append(py_name)
        # If this is a memory view, then access the first element
        # (start of block of memory).
        if ((self.dimension is not None) and (not self.allocatable)):
            names[-1] = "ctypes.c_void_p(" + names[-1] + ".ctypes.data)"
        else:
            names[-1] = "ctypes.byref(" + names[-1] + ")"
        # Return the list of names.
        return names

    # The lines of Python code that must be executed after the Fortran call.
    def py_after(self):
        lines = []
        if self.allocatable and self._is_output():
            py_name = self.name.lower()
            # This must be an array argument and it must be 'INTENT(OUT)'.
            if (self.dimension is None): raise(NotImplementedError)
            # Get the pointer to the first index.
            local_dims = [f"{py_name}_dim_{i+1}.value" for i in range(len(self.dimension))]
            # Compute the size of the (flattened) array.
            lines += [f"{py_name}_size = ({') * ('.join(local_dims)})"]
            shape = ','.join(local_dims[::-1])
            # Make a line that checks if the array should be None.
            if self.optional:
                check_line = [f"if ({py_name}_present) and ({py_name}_size > 0):"]
            else:
                check_line = [f"if ({py_name}_size > 0):"]
            lines += check_line
            # Get flat array pointer and reshape by the dimensions
            # reversed, then transpose (f_contiguous).
            lines += [f"    {py_name} = numpy.array(ctypes.cast({py_name}, ctypes.POINTER({self.c_type}*{py_name}_size)).contents, copy=False)"]
            # If the array type does not match the singleton type, use a view.
            if (self.c_type != self.c_type_array):
                lines[-1] += f".view({self.c_type_array})"
            # If this is a tensor, then reshape it (from C style, row major) to Fortran style.
            if (len(self.dimension) > 1):
                lines += [f"    {py_name} = {py_name}.reshape({shape}).T"]
            # Otherwise, (if not present) this output is None.
            lines += [f"elif ({py_name}_size == 0):",
                      f"    {py_name} = numpy.zeros(shape=({shape}), dtype={self.c_type_array}, order='F')",
                      f"else:",
                      f"    {py_name} = None"]
        # Return all lines.
        return lines

    # The name of the argument in the final "return" line to Python.
    def py_return(self):
        names = []
        if (self._is_output()):
            # Transform array memory views back into NumPy arrays.
            is_pointer = (self._is_output() and self.allocatable)
            # Add this argument to the output.
            py_name = self.name.lower()
            value = py_name
            # Retrieve the Python "value" of the C object if appropriate.
            if (self.dimension is None) and (self.type != "TYPE"):
                value += ".value"
            # Return None for missing optional returns.
            if self.optional: names.append(f"({value} if {py_name}_present else None)")
            else:             names.append(f"{value}")
        # Return the processed name.
        return names

    # ----------------------------------------------------------------
    #                    Module Attribute Access

    # Define a 'getter' function in Python for a public module attribute.
    def py_getter(self):
        py_name = self.name.lower()
        call_args = []
        lines = [f'def get_{py_name}(self):']
        # If this is not in a MODULE, then that was unexpected.
        if ((self.parent == None) or (self.parent.type != "MODULE")):
            raise(NotImplementedError)
        # If this module attribute is allocatable, we might need to return None.
        if (self.allocatable):
            call_args.append(f'ctypes.byref({py_name}_allocated)')
            lines.append(f'    {py_name}_allocated = ctypes.c_bool(False)')
        # Define the dimension size variables if appropriate.
        if (self.dimension is not None):
            for i in range(len(self.dimension)):
                lines.append(f'    {py_name}_dim_{i+1} = ctypes.c_long()')
                call_args.append(f'ctypes.byref({py_name}_dim_{i+1})')
                # Define the actual attribute holder itself.
        if (self.allocatable or (self.dimension is not None)):
            lines += [f'    {py_name} = ctypes.c_void_p()']
        else:
            lines += [f'    {py_name} = {self.c_type}()']
        call_args.append(f'ctypes.byref({py_name})')
        # Make the call to the Fortran wrapped function for getting.
        module_name = self.parent.name.lower()
        lines += [f'    clib.{module_name}_get_{py_name}({", ".join(call_args)})']
        # If this allocatable is not allocated, return "None".
        if (self.allocatable):
            lines.append(f'    if (not {py_name}_allocated.value): return None')
        # If this is an array, retreive its value from a pointer.
        if (self.dimension is not None):
            # Get the pointer to the first index.
            local_dims = [f"{py_name}_dim_{i+1}.value" for i in range(len(self.dimension))]
            # Compute the size of the (flattened) array.
            lines.append(f"    {py_name}_size = ({') * ('.join(local_dims)})")
            # Get flat array pointer and reshape by the dimensions
            # reversed, then transpose (f_contiguous).
            lines.append(f"    if ({py_name}_size > 0):")
            lines.append(f"        {py_name} = numpy.array(ctypes.cast({py_name}, ctypes.POINTER({self.c_type}*{py_name}_size)).contents, copy=False)")
            # If the array type does not match the singleton type, use a view.
            if (self.c_type != self.c_type_array):
                lines[-1] += f".view({self.c_type_array})"
            lines.append(f"    else:")
            lines.append(f"        {py_name} = numpy.zeros((0,), dtype={self.c_type}, order='F')")
            # If this is a tensor, then reshape it to be a tensor again.
            if (len(self.dimension) > 1):
                shape = ','.join(local_dims[::-1])
                lines += [f"    {py_name} = {py_name}.reshape({shape}).T"]
            # Return the value.
            lines += [f'    return {py_name}']
        else:
            lines += [f'    return {py_name}.value']
        # Return the lines that compose this "getter".
        return lines

    # Define a 'setter' function in Python for a public module attribute.
    def py_setter(self):
        py_name = self.name.lower()
        lines = [f'def set_{py_name}(self, {py_name}):']
        if self.parameter:
            lines += ["    raise(NotImplementedError('Module attributes with PARAMETER status cannot be set.'))"]
            return lines
        call_args = []
        # If this is not in a MODULE, then that was unexpected.
        if ((self.parent == None) or (self.parent.type != "MODULE")):
            raise(NotImplementedError)
        # Get the size if there is a dimension.
        if (self.dimension is not None):
            lines += [f"    if ((not issubclass(type({py_name}), numpy.ndarray)) or",
                      f"        (not numpy.asarray({py_name}).flags.f_contiguous) or",
                      f"        (not ({py_name}.dtype == numpy.dtype({self.c_type})))):",
                       "        import warnings",
                      f"        warnings.warn(\"The provided argument '{py_name}' was not an f_contiguous NumPy array of type '{self.c_type}' (or equivalent). Automatically converting (probably creating a full copy).\")",
                      f"        {py_name} = numpy.asarray({py_name}, dtype={self.c_type}, order='F')",]
            # Store all the sizes.
            for i in range(len(self.dimension)):
                dim_name = f'{py_name}_dim_{i+1}'
                lines.append(f'    {dim_name} = ctypes.c_long({py_name}.shape[{i}])')
                call_args.append(f'ctypes.byref({dim_name})')
            # Call passing in all arguments (might include sizes).
            call_args.append(f'ctypes.c_void_p({py_name}.ctypes.data)')
        else:
            # Initialize correct c_type version of value.
            lines.append(f'    {py_name} = {self.c_type}({py_name})')
            # Call passing in all arguments (might include sizes).
            call_args.append(f'ctypes.byref({py_name})')
        module_name = self.parent.name.lower()
        lines += [f'    clib.{module_name}_set_{py_name}({", ".join(call_args)})']
        return lines

    # Define the 'property' referencing the correct getter and setter.
    def py_property(self):
        py_name = self.name.lower()
        module_name = self.parent.name.lower()
        getter_name = f"get_{py_name}"
        setter_name = f"set_{py_name}"
        return [f'{py_name} = property({getter_name}, {setter_name})']

    # Define the FORTRAN BIND(C) subroutine for retreiving the value
    # of this module attribute.
    def fort_getter(self):
        # Add extra arguments for the dimension sizes.
        args = []
        decs = [f"USE {self.parent.name}, ONLY: {self.name}"] # declarations
        lines = []
        # Create a local copy.
        temp = self.copy()
        temp.name += "_LOCAL"
        # Get allocatable present input.
        if (self.allocatable):
            args.append(f'{self.name}_ALLOCATED')
            decs.append(f'LOGICAL, INTENT(OUT) :: {self.name}_ALLOCATED')
            lines += [f'{self.name}_ALLOCATED = ALLOCATED({self.name})',
                      f'IF (.NOT. {self.name}_ALLOCATED) RETURN']

        # Get all dimension size inputs.
        if (self.dimension is not None):
            temp.type = "INTEGER"
            temp.kind = "INT64"
            temp.dimension = None
            temp.allocatable = False
            decs.insert(0,"USE ISO_FORTRAN_ENV, ONLY: INT64")
            for i in range(len(self.dimension)):
                args.append(f"{self.name}_DIM_{i+1}")
                decs.append(f"INTEGER(KIND=INT64), INTENT(OUT) :: {self.name}_DIM_{i+1}")
                lines.append(f"{self.name}_DIM_{i+1} = SIZE({self.name}, {i+1}, KIND=INT64)")
        # Disable "parameter" status for the local copy.
        if self.parameter: temp.parameter = False
        # Add argument for the actual variable.
        args.append(f"{temp.name}")
        decs += [str(temp)]
        # Insert the declarations before the lines.
        lines = decs + lines
        # Do the assignment.
        if (self.dimension is None):
            lines.append(f"{temp.name} = {self.name}")
        else:
            first_pos = ",".join(["1"]*len(self.dimension))
            lines.append(f"{temp.name} = LOC({self.name}({first_pos}))")
        # Add indentation to all lints.
        lines = ["  "+l for l in lines]
        # Add the subroutine line (with all arguments).
        lines.insert(0, f"SUBROUTINE {self.parent.name}_GET_{self.name}({', '.join(args)}) BIND(C)")
        # Add the end of the subroutine declaration line.
        lines.append(f"END SUBROUTINE {self.parent.name}_GET_{self.name}")
        return lines

    # Define the FORTRAN BIND(C) subroutine for setting the value
    # of this module attribute.
    def fort_setter(self):
        # If this is a parameter, it cannot be set.
        if (self.parameter): return []
        # Add extra arguments for the dimension sizes.
        args = []
        lines = []
        # Get all dimension size inputs.
        temp = self.copy()
        temp.name += "_LOCAL"
        temp.dimension = []
        temp.allocatable = False
        if (self.dimension is not None):
            for i in range(len(self.dimension)):
                args.append(f"{self.name}_DIM_{i+1}")
                lines.append(f"INTEGER(KIND=SELECTED_INT_KIND(18)), INTENT(IN) :: {self.name}_DIM_{i+1}")
                temp.dimension.append(args[-1])
        # Add argument for the actual variable.
        args.append(temp.name)
        # Insert the declaration for the local (input / set) value.
        lines += [str(temp)]
        # Get allocatable present input.
        if (self.allocatable):
            lines += [f'IF (ALLOCATED({self.name})) THEN',
                      f'  DEALLOCATE({self.name})',
                       'END IF']
            shape_str = ','.join([f'1:{name}' for name in temp.dimension])
            lines += [f'ALLOCATE({self.name}({shape_str}))']
        lines += [f"{self.name} = {temp.name}"]
        # Add indentation to all lints.
        lines = ["  "+l for l in lines]
        # Add the subroutine line (with all arguments).
        lines.insert(0, f"SUBROUTINE {self.parent.name}_SET_{self.name}({', '.join(args)}) BIND(C)")
        lines.insert(1, f"  USE {self.parent.name}, ONLY: {self.name}")
        # Add the end of the subroutine declaration line.
        lines.append(f"END SUBROUTINE {self.parent.name}_SET_{self.name}")
        return lines


    # ----------------------------------------------------------------
    #                    Wrapper Utilities

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
        import re # <- use regular expressions to capture SIZE(...) syntax.
        sizes = []
        for size in self.dimension:
            size = size.lower().replace(" ","")
            # Replace all occurrences of "SIZE(<argument>)"
            #   with a Python NumPy equivalent array syntax.
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
                # Now "argument" contains whatever was inside the parenthesis
                #  and passed into the `SIZE(...)` function in Fortran.
                argument = argument.strip()
                # If this is just a reference to a variable name...
                if (re.match(r"^\w+$", argument) is not None):
                    func_replacement = f"{argument}.size"
                # If this is formatted as "<variable name>, <integer>", get that dimension..
                elif (re.match(r"^\w+,\s*\d+$", argument) is not None):
                    name = argument[:argument.index(",")]
                    dim = argument[argument.index(",")+1:]
                    dim = str(int(dim) - 1)
                    func_replacement = f"{name}.shape[{dim}]"
                # If this is formatted as "<variable name>(<slice>[,<slice>])", translate the slice(s)..
                elif (re.match(r"^\w+[(]\s*(:|[\w]+)\s*(,\s*(:|[\w]+)\s*)*[)]$", argument) is not None):
                    name = argument[:argument.index("(")]
                    slices = argument[argument.index("(")+1:argument.index(")")].split(",")
                    for i in range(len(slices)):
                        if (slices[i].strip() == ":"): continue
                        try:
                            slices[i] = str(int(slices[i])-1)
                        except ValueError:
                            slices[i] += "-1"
                    func_replacement = f"{name}[{','.join(slices)}].size"
                # Otherwise, the contents of `SIZE(...)` cannot be parsed successfully.
                else:
                    raise(NotImplementedError("The contents of 'SIZE' could not be parsed successfully. If this is valid Fortran syntax, consider raising an issue with a minimum necessary example from this code.\n\n  "+size))
                # Replace float division with integer division in python.
                size = before_call + func_replacement + after_call
            # Append this NumPy size.
            sizes.append(size)
        # fix any module attribute names to have "self." before them
        if (hasattr(self.parent, "parent") and
            hasattr(self.parent.parent, "type") and
            (self.parent.parent.type == "MODULE")):
            # create a temporary function that says whether or
            #  not a character could belong to a python name
            is_not_py_char = lambda c: not (c.isalpha() or c.isdigit() or (c == '_'))
            # cycle the internal attribute of the parent module that
            #  might be accessible here
            for arg in self.parent.parent.arguments:
                name = arg.name.lower()
                # cycle all size entries to check their contents
                for i in range(len(sizes)):
                    # if the module attribute is used and it is not just
                    #  a substring match, then make the replacement
                    s = sizes[i]
                    if (name in s):
                        j = s.index(name)
                        if ((j == 0) or is_not_py_char(s[j-1])):
                            sizes[i] = s.replace(name, "self."+name)
        # Return the list of sizes.
        return sizes

    # ----------------------------------------------------------------
    #                      Generic Methods

    # Generate a deep copy of this argument and return it.
    def copy(self):
        arg = type(self)([self.type])
        for attr in dir(self):
            # Skip hidden attributes.
            if (attr[:1] == "_"): continue
            # TODO: Come up with better general way to handle these special cases.
            if (attr in {"c_type", "c_type_array", "py_type"}): continue
            value = getattr(self, attr)
            # Skip executable attributes.
            if (hasattr(value, "__call__")): continue
            # If this attribute has a ".copy" function, then use it.
            if (hasattr(value, "copy") and hasattr(value.copy, "__call__")):
                value = value.copy()
            # Set the attribute in the new argument.
            setattr(arg, attr, value)
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
            # Read DIMENSION
            elif (line[0] == "DIMENSION"):
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
            # Read INTENT
            elif (line[0] == "INTENT"):
                group, line = pop_group(line[1:])
                if (len(group) == 0): raise(NotImplementedError)
                self.intent = "".join(group)
            # Read OPTIONAL
            elif (line[0] == "OPTIONAL"):
                line.pop(0)
                self.optional = True
            # Read SAVE
            elif (line[0] == "SAVE"):
                line.pop(0)
                self.save = True
            # Read PARAMETER (discard this)
            elif (line[0] == "PARAMETER"):
                line.pop(0)
                self.parameter = True
            # Read EXTERNAL (discard this)
            elif (line[0] == "EXTERNAL"): 
                line.pop(0)
                import warnings
                warnings.warn("fmodpy.parsing.argument: Ignoring 'EXTERNAL' status of argument.")
            # Read LEN attribute (for inside of TYPE declarations)
            elif (line[0] == "LEN"):
                line.pop(0)
                self.type_len = True
            # Read KIND attribute (for inside of TYPE declarations)
            elif (line[0] == "KIND"):
                line.pop(0)
                self.type_kind = True
            # Otherwise, this is an unrecognized argument.
            else:
                raise(NotImplementedError(f"\n\nUnrecognized part of Argument '{line[0]}'.\n"))

    # Print the Fortran string declaration of this argument.
    def __str__(self):
        out = f"{self.type}"
        if (len(self.kind) > 0): out += f"({self.kind_prefix}{self.kind})"
        if (self.show_intent and (len(self.intent) > 0)): out += f", INTENT({self.intent})"
        if (self.type_len): out += ", LEN"
        if (self.type_kind): out += ", KIND"
        if (self.parameter): out += ", PARAMETER"
        if (self.optional): out += f", OPTIONAL"
        if (self.allocatable): out += f", ALLOCATABLE"
        if (self.save): out += f", SAVE"
        if ((self.dimension is not None) and
            (len(self.dimension) > 0)):
            out += f", DIMENSION({','.join(self.dimension)})"
        if (len(self.name) > 0): out += f" :: {self.name}"
        # Return the final string.
        return out
