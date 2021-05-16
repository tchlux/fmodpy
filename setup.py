# Custom error (in case user does not have setuptools)
class DependencyError(Exception): pass

# Try to import setuptools (if it fails, the user needs that package)
try: 
    from setuptools import setup, find_packages
except:
    raise(DependencyError("Missing python package 'setuptools'.\n  python3 -m pip install --user setuptools"))

import os
# Go to the "about" directory in the package directory
PACKAGE_NAME = "fmodpy"
DEFAULT_DIRECTORY = os.path.join(os.path.dirname(os.path.abspath(__file__)),PACKAGE_NAME,"about")
# Convenience function for reading information files
def read(f_name, dir_name=DEFAULT_DIRECTORY, processed=True):
    text = []
    # Check for directory existence.
    if (not os.path.exists(dir_name)):
        print(f"ERROR: No directory found '{dir_name}'")
        while (not os.path.exists(dir_name)):
            dir_name = os.path.dirname(dir_name)
        print(f"       only found {os.listdir(dir_name)}.")
        return [""] if processed else ""
    # Check for path existence.
    path = os.path.join(dir_name, f_name)
    if (not os.path.exists(path)):
        print(f"ERROR: No path found '{path}'")
        print(f"       only found {os.listdir(dir_name)}.")
        return [""] if processed else ""
    # Process the contents and return.
    with open(path) as f:
        if processed:
            for line in f:
                line = line.strip()
                if (len(line) > 0) and (line[0] != "%"):
                    text.append(line)
        else:
            text = f.read()
    # Return the text.
    return text

if __name__ == "__main__":
    #      Read in the package description files     
    # ===============================================
    package = PACKAGE_NAME
    version =read("version.txt")[0]
    description = read("description.txt")[0]
    requirements = read("requirements.txt")
    keywords = read("keywords.txt")
    classifiers = read("classifiers.txt")
    name, email, git_username = read("author.txt")

    setup(
        author=name,
        author_email=email,
        name=package,
        packages=find_packages(exclude=[]),
        include_package_data=True,
        install_requires=requirements,
        version=version,
        url = f"https://github.com/{git_username}/{package}",
        download_url = f"https://github.com/{git_username}/{package}/archive/{version}.tar.gz",
        description = description,
        keywords = keywords,
        python_requires = ">=3.6",
        license="MIT",
        classifiers=classifiers
    )

    # Check to see if the user has a fortran compiler. Create a
    # warning if there is no Fortran compiler installed.

