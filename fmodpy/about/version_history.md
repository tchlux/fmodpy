| 1.1.0<br>December 2020 | Supports MODULE wrapping and basic types. |
| 1.2.0<br>February 2021 | Fixing array size and dimension checks for optional <br> array arguments. Working towards support of types. |
| 1.2.1<br>February 2021 | Fixing install issue with setup.py |
| 1.2.2<br>May 2021 | Added support for complex types. |
| 1.3.0<br>May 2021 | Removed OG fmodpy, added complex256 tests work in <br> progress. |
| 1.3.1<br>June 2021 | Fixing minor bug in argument parsing. |
| 1.3.2<br>June 2021 | Adding ability to parse Fortran files with renamed <br> types. |
| 1.3.3<br>September 2021 | Fixed numerous minor bugs and typos. Added basic <br> support for characters, as long as they have no <br> length. |
| 1.3.4<br>October 2021 | Refactored parsing of SIZE(...) in assumed shape <br> array allocation during wrapping. Now supports <br> slightly more complicated arguments to the SIZE <br> function. |
| 1.4.0<br>January 2022 | Updated documentation parsing to not clear comments <br> on new lines after subroutine definition. Fixed <br> character array tests. Added basic support for <br> Fortran derived types as long as they are BIND(C) <br> already. |
| 1.4.1<br>February 2022 | Fixed usability bugs with derived types, added final <br> directory cleaning, fixed bug in module dependencies <br> that relied on .mod file, found bug in gfortran when <br> trying to support parameterized types. |
| 1.4.2<br>February 2022 | Derived type struct fields in Python are lower case <br> to match python-wrapped behaviors in the rest of the <br> package. |
| 1.4.3<br>March 2022 | Added warnings for long compilation, many optional <br> arguments, and included architecture in compiled <br> file names. |
| 1.4.3<br>March 2022 | Logical byte size adjustment from 03-27 that <br> supports C-bool types with only one byte. |
| 1.4.4<br>March 2022 | Logical byte size adjustment from 03-27 that <br> supports C-bool types with only one byte. |
| 1.4.5<br>April 2022 | Updated size measurements for arrays to 64 bit <br> integers. |
| 1.4.6<br>April 2022 | Name change of depends_files to dependencies. Missed <br> this upload last weekend. |
| 1.4.7<br>April 2022 | Updated dependency handling, automatic compilation <br> now starts with dependencies in order, source code <br> modification times are checked for all dependencies. |
| 1.4.8<br>May 2022 | Added support for types defined in subroutines in <br> standalone files. Updated test case to reflect new <br> addition. Added 'verbose_module' parameter for <br> setting the default verbosity of generated modules. |
| 1.5.0<br>July 2022 | Added the ability to load symbols from system <br> libraries, improves automatic dependency resolution. |
| 1.5.1<br>July 2022 | Minor modifications to fix some compilation and <br> import bugs when supporting generic search for <br> symbols. Symbol dependencies are now loaded within <br> the generated wrapper, to ensure correct operation <br> after the initial build. |
| 1.5.2<br>July 2022 | Added some additional standard library search paths <br> and renamed 'delete_destination' as 'overwrite'. <br> Also changed default configurations that were <br> strings being split into proper Python lists. |
| 1.5.3<br>March 2023 | Minor update. Changed some logic in fimport to more <br> correctly support different system configurations. <br> Added comments drescribing most configurable <br> settings. Added support for TARGET and POINTER <br> types. |
| 1.5.4<br>March 2023 | Added support for '$' as a line continuation <br> character in the fifth column of fixed format files. <br> Switched 'LOGICAL' types to be 'C_BOOL' |
| 1.5.5<br>March 2023 | Updated error message for unnamed END statements to <br> include configuration suggestion. Made fixed format <br> Fortran files set 'end_is_named=False' by default. |
| 1.5.6<br>March 2023 | Making some 'os.remove' operations safer by checking <br> for path existence. Refactored 'f_compiler_args' <br> into two more parts, 'optimization_level' and <br> 'shared_object_args', so that the typical '-fPIC <br> -shared' do not need to be included when adding <br> custom compilation arguments. |
| 1.5.6<br>March 2023 | Making some 'os.remove' operations safer by checking <br> for path existence. Refactored 'f_compiler_args' <br> into two more parts, 'optimization_level' and <br> 'shared_object_args', so that the typical '-fPIC <br> -shared' do not need to be included when adding <br> custom compilation arguments. |
| 1.5.7<br>April 2023 | Delayed warning message about LOGICAL arrays until <br> those arguments actually have a python interface <br> generated. This is to prevent parsed-but-unused <br> subroutine arguments from creating noise. |
| 1.6.0<br>June 2023 | Added support for Fortran strings. Updated the <br> generated codes to automatically check for source <br> code modifications when loading and recompile if the <br> source code was modified. |
| 1.7.0<br>November 2023 | Added support for parsing PARAMETER lines in fixed <br> format Fortran. |
| 1.7.1<br>January 2024 | Fixed bug in type of return argument for allocatable <br> arrays. Updated messaging about implicit typing to <br> be more useful. |
| 1.7.2<br>March 2024 | Updating command execution to use a list of <br> arguments instead of string join and 'shell=True'. <br> Moved compile options to the end of the argument <br> list to ensure that libraries are included correctly <br> in compiled shared object. |
| 1.7.3<br>March 2024 | A commensurate update to size check compilations <br> that was made in the previous update. Moving library <br> specification to the back of the list of arguments <br> for the compiler to ensure dependencies are included <br> correctly. |
| 1.7.4<br>May 2024 | Add support for 'size(a,1,kind=int64)' style size <br> specifications of inputs. |
| 1.7.5<br>January 2026 | Removing deprecated 'pkgutil' in favor of <br> 'importlib.util' for python3.14+ support. |
