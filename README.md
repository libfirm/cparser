cparser - A C99 parser (with gnu extensions)
============================================

1. Introduction
---------------

cparser is a recursive descent C99 parser written in C99. It contains
preprocessor, lexer, parser, constructs an AST and does semantic analysis.  It
acts as a frontend to the libFirm intermediate representation, optimization and
code generation. It comes with typical compiler driver logic for parsing the
commandline to setup the environment and code generator and calling assemblers
and linkers. This allows it to be a drop-in replacement for gcc or clang in
many situations.

2. Building and Installation
----------------------------

Requirements:

* A C99 compiler (gcc and icc are known to work).
* pkg-config (recommended)
* libFirm-1.21 or later

Make sure you have installed libFirm and pkg-config can find the libfirm.pc
files ("pkg-config --modversion libfirm" should work). Use (GNU)-make to build
cparser.

3. Troubleshooting
------------------

x86\_64 Systems:
libFirm uses a 32bit x86 backend by default, while it also uses the systems
default preprocessor/linker. This results on a x86\_64 linker being used
for 32bit assembly. If you see assembler errors like
"suffix or operand invalid for 'push'", then you probably have this problem.
Use the -m32 flag to force usage of system 32bit preprocessor/linker.

4. Contact
----------

There's a Bugtracker at http://pp.info.uni-karlsruhe.de/~firm/bugs
You can contact me at matze@braunis.de
You might also visit the #firm channel on irc.freenode.net
