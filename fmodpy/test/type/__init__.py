
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
                          output_dir=dir_name, rebuild=True,
    )
    # ---------------------------------------------------------------
    # Begin specific testing code.

    t1 = fort.ftypes.T1()
    t2 = fort.ftypes.copy_t1_to_t2(t1)
    assert((t1.a == t2.a) and (t1.b == t2.b))

    import numpy as np
    n = 10
    array_in = np.asarray(np.arange(n), dtype=fort.ftypes.T1, order='F')
    array_out = np.zeros(n//2 - 1, dtype=fort.T2, order='F')
    out = fort.test_standard(7, array_in, array_out)
    # Test the standard functionality.
    assert(out[0].a == out[0].b == 8)
    assert(all(out[1]["a"] == [0,1,2,3]))
    assert(all(out[1]["b"] == [0,1,2,3]))
    assert(all(out[2]["a"] == [1,2,3,4]))
    assert(all(out[2]["b"] == [1,2,3,4]))
    assert(np.all(out[3]["a"] == [[2,3,4,5],
                                  [3,4,5,6],
                                  [4,5,6,7]]))
    assert(np.all(out[3]["b"] == [[2,3,4,5],
                                  [3,4,5,6],
                                  [4,5,6,7]]))
    assert(out[4] == None)
    # Test the extended functionality.
    out = fort.test_extended(4)
    assert(out[0] == None)
    assert(out[1] == None)
    assert(all(out[2]["a"] == [4, 3, 2, 1]))
    assert(all(out[2]["b"] == [4, 3, 2, 1]))
    # WARNING: In the current code, the memory associated with out[2]
    #          will be freed by Fortran on subsequent calls to the
    #          function "test_extended". Copy for object permanance.
    out2 = fort.test_extended(10, known_opt_array_out=True)
    assert(all(out2[0]["a"] == [1,2,3]))
    assert(all(out2[0]["b"] == [1,2,3]))
    assert(out2[1] == None)
    assert(all(out2[2]["a"] == list(reversed(range(11)[1:]))))
    assert(all(out2[2]["b"] == list(reversed(range(11)[1:]))))
    # WARNING: Similar to before, the memory at out2[0] and out2[2]
    #          will be freed by Fortran on subsequent calls to
    #          "test_extended". Copies should be made for permanence.
    out3 = fort.test_extended(6, opt_alloc_array_out=True)
    assert(out3[0] == None)
    assert(all(out3[1]["a"] == [3,2,1]))
    assert(all(out3[1]["b"] == [3,2,1]))
    assert(all(out3[2]["a"] == list(reversed(range(7)[1:]))))
    assert(all(out3[2]["b"] == list(reversed(range(7)[1:]))))

    # End specific testing code.
    # ---------------------------------------------------------------
    print("passed", flush=True)
    import shutil
    shutil.rmtree(os.path.join(dir_name,f"test_{test_name}"))


if __name__ == "__main__":
    test()
