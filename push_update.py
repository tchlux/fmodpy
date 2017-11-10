class CommandError(Exception): pass
class NotEnoughArguments(Exception): pass

# Execute a blocking command with a subprocess, raise errors if any
# are encountered, otherwiser print standard output and return. This
# should work across both Python2.7 and Python3.x as well as cross-platform.
#  INPUT:
#   command -- A list of strings or string (space separated) describing
#              a standard command as would be given to subprocess.Popen
def run(command, **popen_kwargs):
    import sys, subprocess
    # For Python3.x ensure that the outputs are strings
    if sys.version_info >= (3,6):
        popen_kwargs.update( dict(encoding="UTF-8") )
    # print("'%s'"%(" ".join(command)))
    # return
    proc = subprocess.Popen(command, stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE, **popen_kwargs)
    stdout, stderr = proc.communicate()
    if stdout: stdout = stdout.replace("\r","").split("\n")
    else:      stdout = ""
    if stderr: stderr = stderr.replace("\r","").split("\n")
    else:      stderr = ""
    if (proc.returncode != 0) or (len(stderr) > 0):
        raise(CommandError("\n\n%s"%("\n".join(stderr))))
    elif (len(stdout) > 0):
        print("\n".join(stdout))


DRY_RUN = False
CLEAN_BEFORE = True
UPDATE_README = not DRY_RUN
GIT_UPDATE = not DRY_RUN
GIT_RELEASE = not DRY_RUN
AUTO_MANIFEST = True
MANIFEST_EXCLUSIONS = [".git", ".gitignore"]
PYPI_BUILD = True
PYPI_RELEASE = not DRY_RUN
# CLEAN_AFTER = not DRY_RUN
CLEAN_AFTER = True
if __name__ == "__main__":
    # Read the package name
    import os
    package = os.path.basename(os.path.dirname(os.path.abspath(__file__)))
    from setup import read
    version = read("version.txt")[0]
    
    if CLEAN_BEFORE:
        #      Remove all of the wheel generated files     
        # =================================================
        run(["rm", "-rf", "dist", "build", package+".egg-info"])
        # Remove any pyc files that are hidden away     
        run(["find", ".", "-name", '*.pyc', "-delete"])
        run(["find", ".", "-name", '__pycache__', "-delete"])

    if (UPDATE_README or GIT_UPDATE or GIT_RELEASE):
        # Make sure the user provided the correct number of arguments
        import sys, datetime
        if len(sys.argv) >= 2:
            notes = sys.argv[1]
        else:
            raise(NotEnoughArguments("Provide update notes as command line argument."))

    if UPDATE_README:
        #      Update the readme with the notes for this update     
        # ==========================================================
        max_comment_length = 52
        formatted_comment = notes.split()
        start = 0
        curr = 1
        while (curr < len(formatted_comment)):
            if len(" ".join(formatted_comment[start:curr+1])) > max_comment_length:
                formatted_comment.insert(curr, "<br>")
                start = curr+1
            curr += 1
        formatted_comment = " ".join(formatted_comment)
        # Insert the comment into the table in the readme
        contents = []
        inserted = False
        found_top_of_version_block = False
        with open("readme.md") as f:
            for line in f:
                if not inserted:
                    search = line.strip()
                    if ("Version and Date" in search) and ("Description" in search):
                        found_top_of_version_block = True
                    if ((len(search) == 0) and found_top_of_version_block):
                        month = datetime.datetime.now().strftime("%B")
                        year = datetime.datetime.now().strftime("%Y")
                        time = version +"<br>"+ month +" "+ year
                        contents.append(
                            "| %s | %s |\n"%(time, formatted_comment) )
                        # Insert the new information
                        inserted = True
                        print(contents[-1])
                # Add the line to the output
                contents.append(line)
        with open("readme.md", "w") as f:
            f.write("".join(contents))

    if AUTO_MANIFEST:
        with open("MANIFEST.in", "w") as f:
            for name in os.listdir(os.getcwd()):
                if name not in MANIFEST_EXCLUSIONS:
                    if os.path.isdir(name):
                        print("recursive-include",name,"*", file=f)
                    else:
                        print("include",name, file=f)

    if GIT_UPDATE:
        #      Upload current version with git     
        # =========================================
        run(["git", "add", "*"])
        run(["git", "commit", "-a", "-m", notes])
        run(["git", "push", package, "master"])

    if GIT_RELEASE:
        #      Upload to github with version tag     
        # ===========================================
        run(["git", "tag", "-a", version, "-m", notes])
        run(["git", "push", "--tags"])

    if PYPI_BUILD:
        #      Setup the python package as a universal wheel     
        # =======================================================
        run(["python3", "setup.py", "sdist"])
        # run(["python3", "setup.py", "bdist_wheel"])
    
        #      Remove any pyc files that are hidden away     
        # ===================================================
        run(["find", ".", "-name", '*.pyc', "-delete"])
        run(["find", ".", "-name", '__pycache__', "-delete"])

    if PYPI_RELEASE:
        #      Use twine to upload the package to PyPI     
        # =================================================
        run(["twine", "upload", "dist/*"])

    if CLEAN_AFTER:
        #      Remove all of the wheel generated files     
        # =================================================
        run(["rm", "-rf", "dist", "build", package+".egg-info", "MANIFEST.in"])

