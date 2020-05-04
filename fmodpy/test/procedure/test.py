import fmodpy
import og_fmodpy

# fort = og_fmodpy.fimport("procedure.f03", verbose=True,
#                          working_directory="og_procedure",
#                          autocompile_extra_files=True)
# help(fort)

fort = fmodpy.fimport("procedure.f03", build_dir="fmodpy_procedure",
                      verbose=True, wrap=True, rebuild=True)
help(fort)

