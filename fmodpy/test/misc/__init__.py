
# Standardized testing interface.
def test():
    import os
    import shutil
    dir_name = os.path.dirname(os.path.abspath(__file__))
    test_name = os.path.basename(dir_name)
    build_dir = os.path.join(dir_name, f"fmodpy_{test_name}")
    import fmodpy
    # ---------------------------------------------------------------
    # Begin specific testing code.
    fort_file = os.path.join(dir_name, f"subroutine_with_type.f90")
    print(f"  {os.path.basename(fort_file)}..", end=" ", flush=True)
    fort = fmodpy.fimport(fort_file, build_dir=build_dir,
                          output_dir=dir_name, rebuild=True,
    )
    assert (fort.one_up(1) == fort.FANCY(shirt=2.0, pants=3)), "Failed!"
    print("passed", flush=True)
    shutil.rmtree(os.path.join(dir_name,fort_file[:-4]))
    # End specific testing code.
    # ---------------------------------------------------------------


if __name__ == "__main__":
    test()
