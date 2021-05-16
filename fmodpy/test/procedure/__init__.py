

# Standardized testing interface.
def test():
    import os
    dir_name = os.path.dirname(os.path.abspath(__file__))
    test_name = os.path.basename(dir_name)
    fort_file = os.path.join(dir_name, f"{test_name}.f03")
    build_dir = os.path.join(dir_name, f"fmodpy_{test_name}")
    print(f"  {test_name}..", end=" ", flush=True)
    import fmodpy
    fort = fmodpy.fimport(fort_file, build_dir=build_dir,
                          output_dir=dir_name, verbose=False, wrap=True,
                          rebuild=True, show_warnings=False)
    # ---------------------------------------------------------------
    # Begin specific testing code.

    import numpy as np
    a = np.array([True, False, True, False, True, False, True, False], dtype=bool)
    temp = np.asarray(a, dtype='int32')
    out = temp.copy()
    assert(all(out == fort.test_simple_logical(temp, b=out, c=False)))
    assert(not any(fort.test_simple_logical(temp, b=out, c=True)))

    # End specific testing code.
    # ---------------------------------------------------------------
    print("passed", flush=True)


if __name__ == "__main__":
    test()

