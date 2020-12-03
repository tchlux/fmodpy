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

    import numpy as np
    n = 10
    array_test = np.asarray(np.arange(n), dtype="float32", order='F')

    # Assign some internal variables.
    #   a_pub (float)
    fort.addition.a_pub = 1.5
    assert(fort.addition.a_pub == 1.5)
    fort.addition.a_pub = 2.5
    assert(fort.addition.a_pub == 2.5)

    #   b_pub (integer)
    fort.addition.b_pub = 2
    assert(fort.addition.b_pub == 2)
    fort.addition.b_pub = 1
    assert(fort.addition.b_pub == 1)

    # Verify that the public subtraction routine works correctly.
    assert(3.5 == fort.addition.add_ab())

    #   a_vec_pub (real vector of size 10)
    import numpy as np
    a = np.asarray(np.random.random(size=(10,)), dtype=np.float32)
    # testing using the "get" before assignment
    fort.addition.a_vec_pub 
    # test assignment.
    fort.addition.a_vec_pub = a
    assert(all(a == fort.addition.a_vec_pub))
    # try assigning with a vector that is too small..
    a = np.asarray(np.random.random(size=(8,)), dtype=np.float32)
    fort.addition.a_vec_pub = a
    assert(all(a == fort.addition.a_vec_pub[:len(a)]))
    # try assigning with a vector that is too big, might seg-fault..
    a = np.asarray(np.random.random(size=(80,)), dtype=np.float32)
    fort.addition.a_vec_pub = a
    assert(all(a[:len(fort.addition.a_vec_pub)] == fort.addition.a_vec_pub))
    # reassign a normal value.
    a = np.asarray(np.random.random(size=(10,)), dtype=np.float32)
    fort.addition.a_vec_pub = a

    # Make sure the vector is correctly negated internally and returned.
    result = fort.addition.sub_vecs()
    assert(all(result == -a))

    #   b_vec_pub (real vector of size 10)
    b = np.asarray(np.arange(4), dtype=np.float32)
    # testing using the "get" before assignment
    # test assignment.
    fort.addition.b_vec_pub = b
    assert(all(b == fort.addition.b_vec_pub))
    # try assigning over top with a new vector
    b = np.asarray(np.arange(10), dtype=np.float32)
    fort.addition.b_vec_pub = b
    assert(all(b == fort.addition.b_vec_pub))

    # Make sure the vectors are correctly subtracted internally and returned.
    result = fort.addition.sub_vecs()
    assert(all(b == result + a))

    # TODO: check size of array, if large enough, warn about copy in SETTER
    # TODO: provide some protection against incorrectly sized array assignments

    # End specific testing code.
    # ---------------------------------------------------------------
    print("passed", flush=True)
    import shutil
    shutil.rmtree(os.path.join(dir_name,f"test_{test_name}"))

if __name__ == "__main__":
    test()
