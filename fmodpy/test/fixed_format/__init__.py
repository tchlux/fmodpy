# Standardized testing interface.
def test():
    import os
    dir_name = os.path.dirname(os.path.abspath(__file__))
    test_name = os.path.basename(dir_name)
    fort_file = os.path.join(dir_name, f"test_{test_name}.f")
    build_dir = os.path.join(dir_name, f"fmodpy_{test_name}")
    print(f"  {test_name}..", end=" ", flush=True)
    import fmodpy
    fort = fmodpy.fimport(fort_file, build_dir=build_dir,
                          output_dir=dir_name)
    # ---------------------------------------------------------------
    # Begin specific testing code.
    import numpy as np
    a = 1
    b = 2
    c = 3
    d = 4
    aa = np.zeros((2,10), dtype="int32", order="F")
    ab = np.zeros((3,11), dtype="int32", order="F")
    ac = np.zeros((12), dtype="int32", order="F")
    ad = np.zeros((13), dtype="int32", order="F")
    ba = np.zeros((c,d), dtype="int32", order="F")
    bb = np.zeros((6,14), dtype="int32", order="F")
    # Call the function.
    a, b, c, d, aa, ab, ac, ad, ba, bb = fort.test_standard(a, b, c, d, aa, ab, ac, ad, ba, bb)
    # Test the standard functionality.
    assert(a == 1)
    assert(b == 2)
    assert(c == 3)
    assert(d == 4)
    assert(aa[1,2] == 1)
    assert(ab[1,2] == 2)
    assert(ac[1] == 3)
    assert(ad[1] == 4)
    assert(ba[1,2] == 5)
    # End specific testing code.
    # ---------------------------------------------------------------
    print("passed", flush=True)
    import shutil
    shutil.rmtree(os.path.join(dir_name,f"test_{test_name}"))

if __name__ == "__main__":
    test()
