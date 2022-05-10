# - make immutable INTENT(OUT) objects not part of input.
# - make test cases (and necessary code) for:
#     real
#        inputs
#        outputs
#        array inputs
#        array outputs (known size)
#        array outputs (unknown size)
#        optional inputs
# 
#        optional array inputs
#        optional array outputs (known size)
#        optional array outputs (unknown size)
#        allocatable outputs
#        optional allocatable outputs
#     double precision
#        ^^
#     integer
#        ^^
#     logical
#        ^^
#     character
#        ^^
#     type
#        ^^
#     undefined inputs
#     procedure inputs
#     POINTER attribute
#     modules with interfaces
#     modules with internal variables (define getter / setter functions)
#     modules with custom type declarations
#     type EXTENDS (copy the other type, add more definitions)
#     fixed format Fortran inputs (Fortran 77)
# 
#     
# "Module" should be put inside a Python class. Then I can define
# proper "getter" and "setter" behaviors for internal attributes.
# 


def run_tests():
    from fmodpy.config import load_config
    load_config(verbose=False, verbose_module=False, wrap=True, rebuild=True)

    # Import all of the tests.
    from fmodpy.test.character import test as test_character
    from fmodpy.test.real32 import test as test_real32
    from fmodpy.test.real64 import test as test_real64
    from fmodpy.test.double_precision import test as test_double_precision
    from fmodpy.test.int32 import test as test_int32
    from fmodpy.test.int64 import test as test_int64
    from fmodpy.test.logical import test as test_logical
    from fmodpy.test.module import test as test_module
    from fmodpy.test.complex64 import test as test_complex64
    from fmodpy.test.complex128 import test as test_complex128
    from fmodpy.test.type import test as test_type
    from fmodpy.test.misc import test as test_misc

    # Run all of the tests.
    test_character()
    test_real32()
    test_real64()
    test_double_precision()
    test_int32()
    test_int64()
    test_logical()
    test_module()
    test_complex64()
    test_complex128()
    test_type()
    test_misc()


if __name__ == "__main__":
    # /Users/thomaslux/Library/Python/3.7/bin/pprofile 
    # 
    # Activate a code coverage module.
    # import coverage, os
    # this_dir = os.path.dirname(os.path.abspath(__file__))
    # module_dir = os.path.dirname(this_dir)
    # omitted = ([this_dir] + [os.path.join(this_dir,f) for f in os.listdir(this_dir)]
    #            + [os.path.join(this_dir,f)+"*" for f in os.listdir(this_dir)
    #               if os.path.isdir(os.path.join(this_dir,f))])
    # print("Omitted:")
    # print(omitted)
    # cov = coverage.Coverage(source=[module_dir], omit=omitted)
    # cov.start()

    # Run the tests on `fmodpy`.
    run_tests()

    exit()

    print()
    print("Generating code coverage report..", flush=True)
    # Save data from the coverage tracking (for report).
    cov.stop()
    cov.save()
    print("  (will host local webserver for 10 minutes or until killed)", flush=True)
    # Create a temporary directory for holding test results.
    from tempfile import TemporaryDirectory
    temp_dir = TemporaryDirectory()
    results_dir = temp_dir.name
    cov.html_report(directory=results_dir)
    # Open the results file.
    import webbrowser
    webbrowser.open("file://"+os.path.join(results_dir, "index.html"))
    # Wait for load, then delete the temporary directory.
    #   (user might decide to kill this process early).
    import time
    time.sleep(60*10) # <- Default to 10 minutes of waiting.
    temp_dir.cleanup()
    del temp_dir
