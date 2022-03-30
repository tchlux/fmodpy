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

    import ctypes
    import numpy as np
    a = np.array([True, False, True, False, True, False, True, False], dtype="int32")
    out = a.copy().astype(ctypes.c_bool)
    # Construct the answers (that are expected).
    b12 = np.array([(i+1)%3 == 0 for i in range(len(a))])
    b3  = np.array([(i+1)%2 == 0 for i in range(len(a))])
    assert(all(b12 == fort.test_simple_logical(a, b=out)))
    assert(all(b12 == fort.test_simple_logical(a, b=out, c=False)))
    assert(all(b3  == fort.test_simple_logical(a, b=out, c=True)))
    # End specific testing code.
    # ---------------------------------------------------------------
    print("passed", flush=True)
    import shutil
    shutil.rmtree(os.path.join(dir_name,f"test_{test_name}"))

if __name__ == "__main__":
    test()
