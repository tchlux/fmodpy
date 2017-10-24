# Execute a blocking command with a subprocess, on completion provide
# the return code, stdout as string, and stderr as string. This should
# work across both Python2.7 and Python3.x as well as cross-platform.
#  INPUT:
#   command -- A list of strings or string (space separated) describing
#              a standard command as would be given to subprocess.Popen
# 
#  OUTPUT:
#   return_code -- Straight from the subprocess returncode
#   stdout      -- A list of strings that are the lines of stdout 
#                  produced by the execution of <command>
#   stderr      -- A list of strings that are the lines of stderr
#                  produced by the execution of <command>
def run(command, **popen_kwargs):
    import sys, subprocess
    # For Python3.x ensure that the outputs are strings
    if sys.version_info >= (3,6):
        popen_kwargs.update( dict(encoding="UTF-8") )
    print("'%s'"%(" ".join(command)))
    return 0, [], []
    proc = subprocess.Popen(command, stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE, **popen_kwargs)
    stdout, stderr = proc.communicate()
    if stdout: stdout = stdout.replace("\r","").split("\n")
    else:      stdout = ""
    if stderr: stderr = stderr.replace("\r","").split("\n")
    else:      stderr = ""
    return proc.returncode, stdout, stderr

class NotEnoughArguments(Exception): pass

if __name__ == "__main__":
    import sys
    if len(sys.argv) >= 2:
        notes = sys.argv[1]
    else:
        raise(NotEnoughArguments("Provide update notes as command line argument."))
    # Read the version
    with open("version.txt") as f:
        version = f.read().strip()

    #      Setup the python package as a universal wheel     
    # =======================================================
    code, stdout, stderr = run(["python3", "setup.py", "bdist_wheel"])
    if code != 0:
        print("\n".join(stderr))
    # else:
    #     print("\n".join(stdout))

    #      Remove any pyc files that are hidden away     
    # ===================================================
    code, stdout, stderr = run(["find", ".", "-name", '*.pyc', "-delete"])
    if code != 0:
        print("\n".join(stderr))
    # else:
    #     print("\n".join(stdout))

    #      Upload to github with version tag     
    # ===========================================
    code, stdout, stderr = run(["git", "tag", "-a", version, "-m", notes])
    code, stdout, stderr = run(["git", "push", "--tags"])

    #      Use twine to upload the package to PyPI     
    # =================================================
    code, stdout, stderr = run(["twine", "upload", "dist/*"])
    if code != 0:
        print("\n".join(stderr))
    # else:
    #     print("\n".join(stdout))

    #      Remove all of the wheel generated files     
    # =================================================
    code, stdout, stderr = run(["rm", "-r", "dist", "build", "*.egg-info"])
