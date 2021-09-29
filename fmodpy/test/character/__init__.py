# Standardized testing interface.
def test():
    import os
    dir_name = os.path.dirname(os.path.abspath(__file__))
    test_name = os.path.basename(dir_name)
    fort_file = os.path.join(dir_name, f"test_{test_name}.f03")
    build_dir = os.path.join(dir_name, f"fmodpy_{test_name}")
    print(f"  {test_name}..", end=" ", flush=True)
    import fmodpy
    fort = fmodpy.fimport(fort_file, build_dir=build_dir,
                          output_dir=dir_name, verbose=False, wrap=True,
                          rebuild=True, show_warnings=False)
    # ---------------------------------------------------------------
    # Begin specific testing code.

    import numpy as np
    a = np.array(['1', '0', '1', '0', '1', '0', '1', '0'], dtype='uint8', order='F')
    temp = np.asarray(a, dtype='uint8', order='F')
    out = temp.copy()
    print(fort.test_simple_character(temp, b=out, c=False))
    assert(all(out == fort.test_simple_character(temp, b=out, c=False)))
    assert(not any(fort.test_simple_character(temp, b=out, c=True)))

    # End specific testing code.
    # ---------------------------------------------------------------
    print("passed", flush=True)
    import shutil
    shutil.rmtree(os.path.join(dir_name,f"test_{test_name}"))

if __name__ == "__main__":
    test()
