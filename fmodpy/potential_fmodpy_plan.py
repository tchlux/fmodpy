# fmodpy is a lightweight, efficient, highly-automated, fortran wrapper for python.
# 
# This program is designed to processes a source fortran file into the
# following structure and wrap all contained code into a python module
# 
#   wrap( "<fortran file name>.<extension>", 
#         "<build directory>"="fmodpy_<name>",
#         "<config file>"="fmodpy.config",
#         "<python module name>"="<fortran file name>",
#         c_compiler=None, c_linker="gcc", c_linker_options=None,
#         f_compiler="gfortran", f_compiler_options=None,
#         disallowed_linker_options=None,
#         cleanup=True, verbose=True )
# 
#   FortranModule()
#     .name = "<name>"
#     .use = ["<module 1>", ...]
#     .docs = <documentation>
#     .is_public = True
#     .public = ["<FortranCode name>", ...]
#     .private = ["<FortranCode name>", ...]
#     .subroutines = [Subroutine(), ...]
#     .functions = [Function(), ...]
#     .interfaces = [Interface(), ...]
#     .build_directory = "<build directory>"
#     .fort_compiler = "<fort compiler>"
#     .fort_compiler_options = ["<option 1>", ...]
#     .c_compiler = "<c compiler>"
#     .c_compiler_options = ["<option 1>", ...]
#     .c_linker = "<c linker>"
#     .c_linker_options = ["<option 1>", ...]
#     .parse_file( "<source file name to process>" )
#     .evaluate_sizes()
#     .check_errors()
#     .print()
#     .generate_cython()
#     .generate_fortran()
#     .build()
#     .cleanup()
#     .config("<path to file with configuration settings>"="./fmodpy.config")
# 
#   FortranCode()
#     .module = FortranModule()
#     .name = "<name>"
#     .docs = "<documentation>"
#     .arguments = [FortranArgument(), ...]
#     .parse_line( "<line of source fortran code>" )
#     .generate_cython()
#     .generate_fortran()
#     
#   Subroutine(FortranCode)
#     .parse_line( "<line of source fortran code>" )
#     .generate_cython()
#     .generate_fortran()
# 
#   Function(FortranCode)
#     .return = FortranArgument()
#     .parse_line( "<line of source fortran code>" )
#     .generate_cython()
#     .generate_fortran()
# 
#   Interface(FotranCode)
#     .parse_line( "<line of source fortran code>" )
#     .generate_cython()
#     .generate_fortran()
# 
#   FortranArgument()
#     .name = "<name>"
#     .type = "<type>"
#     .size = "<size>"
#     .kind = "<kind>"
#     .intent = "<intent>"
#     .message = "<messages to user>"
#     .defined = True / False
#     .allocatable = True / False
#     .optional = True / False
#     .dim = ["<dim 1>", ...]
#     ._is_optional()
#     ._is_present()
#     ._to_copy_args()
#     ._to_needed_dim()
#     .to_py_input_doc_string()
#     .to_py_output_doc_string()
#     .to_py_input()
#     .to_py_prepare()
#     .to_py_call()
#     .to_py_after()
#     .to_py_return()
#     .to_c_input()
#     .to_c_prepare()
#     .to_c_call()
#     .to_c_after()
#     .to_fort_input()
#     .to_fort_declare()
#     .to_fort_prepare()
#     .to_fort_call()
#     .to_fort_after()
#
#   Real(FortranArgument)
# 
#   Integer(FortranArgument)
# 
#   Logical(FortranArgument)
# 
#   Character(FortranArgument)
#     .len = "<len>"
# 
#   Procedure(FortranArgument)
#     .procedure = "<procedure name>"
#     .interface = Interface()
