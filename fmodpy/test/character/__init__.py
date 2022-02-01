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
    a = np.array(list(map(ord,['1', '0', '1', '0', '1', '0', '1', '0'])),
                 dtype=ctypes.c_char, order='F')
    out = a.copy()
    # Check for successful copy.
    result = fort.test_simple_character(a, b=out, c=ord('1'))
    assert(all(result.view('uint8') == a.view('uint8')))
    # Check for successful overwrite.
    result = fort.test_simple_character(a, b=out, c=ord('0'))
    assert(all(result.view('uint8') == [1, 2, 0, 1, 2, 0, 1, 2]))

    # End specific testing code.
    # ---------------------------------------------------------------
    print("passed", flush=True)
    import shutil
    shutil.rmtree(os.path.join(dir_name,f"test_{test_name}"))

if __name__ == "__main__":
    test()
