class CommandError(Exception): pass
class NotEnoughArguments(Exception): pass

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
    else:
        print("\n".join(stdout))

if __name__ == "__main__":
    import sys, datetime
    if len(sys.argv) >= 2:
        notes = sys.argv[1]
    else:
        raise(NotEnoughArguments("Provide update notes as command line argument."))
    # Read the version
    with open("version.txt") as f:
        version = f.read().strip()
    
    #      Remove all of the wheel generated files     
    # =================================================
    run(["rm", "-rf", "dist", "build", "fmodpy.egg-info"])

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

    #      Upload to github with version tag     
    # ===========================================
    run(["git", "add", "*"])
    run(["git", "commit", "-a", "-m", notes])
    run(["git", "push", "fmodpy", "master"])
    run(["git", "tag", "-a", version, "-m", notes])
    run(["git", "push", "--tags"])

    #      Setup the python package as a universal wheel     
    # =======================================================
    run(["python3", "setup.py", "bdist_wheel"])
    
    #      Remove any pyc files that are hidden away     
    # ===================================================
    run(["find", ".", "-name", '*.pyc', "-delete"])
    run(["find", ".", "-name", '__pycache__', "-delete"])
    
    #      Use twine to upload the package to PyPI     
    # =================================================
    run(["twine", "upload", "dist/*"])
    
    #      Remove all of the wheel generated files     
    # =================================================
    run(["rm", "-rf", "dist", "build", "fmodpy.egg-info"])
    
