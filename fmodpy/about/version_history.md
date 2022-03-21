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
