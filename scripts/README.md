# LinguaQuanta Scripts

LinguaQuanta provides translations between Quipper and many variants of the
OpenQASM language (e.g., version 2.0, version 3, and the Lattice Surgery 
Compiler inpute language). Certain translations requiring chaining many
LinguaQuanta tools together using the correct choice of arguments. Such
translations are automated by scripts in this directory.

Each script expects a flag `-s` to specify the source file and a flag `-d` to
specify the output destination. An optional flag `-t` can be used to specify a
temporary directory. Intermediate results will be written to the temporary
directory, and are often useful for debugging purposes.
