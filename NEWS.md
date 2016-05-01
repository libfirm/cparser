cparser 1.22.1 (2016-01-23)
---------------------------

* Add make install target
* Stub support for `-Oz`
* Accept `__attribute` as alias for `__attribute__`
* Update online help
* Support `--sysroot` and `-isysroot`
* Enable `-Wunreachable-code`, `-Wunused-function`, `-Wunused-label`, `-Wunused-parameter` and `-Wunused-variable` by default
* Rename `-Wsystem` to `-Wsystem-headers` for gcc/clang compatibility
* Support `-Wcpp`
* Stub support for `#ident` and `#sccs`
* Add switch `-Wreturn-local-addr` and handle compound literals
* Make it an error to have a computed goto in a function without address-taken label
* Add option `-Wdeclaration-after-label`
* Add option `-Wlabel-at-end-of-block`
* Split `-Wparentheses` into `Wparentheses-assignment`, `Wparentheses-comparison`, `Wparentheses-else`, `Wparentheses-logical` and `Wparentheses-shift`
* Bugfixes

cparser 1.22.0 (2015-12-31)
---------------------------

* Bump version number to somewhat match libfirm
* Color diagnostics
* Full preprocessor support
* Support C99 complex numbers
* Improved commandline option handling emulating even more gcc options
* Create libcparser to facilitate the creation of external tools
* Bugfixes
* Improved diagnostics
* Support -Wa, -Xlinker, -Xpreprocessor, -Xassembler
* Support -Wsystem
* Partial support for C11 features
* Support -pedantic and -pedantic-errors
* Support `__attribute__((alias()))`
* Support -Wnot-compound-assign
* Support -Wcompat-option
* Support -Wunknown-warning-option
* Select the base type for enums like gcc
* Support -no-integrated-cpp
* Support -Wcomment (warn about `/*` within `/**/` comments)
* Use integrated preprocessor by default
* Improve auto-detection of host machine triple
* Support -Wunused-option
* Support -Wenum-conversion
* Improve -Wsign-compare
* Support -Wdistinct-pointer-types
* Make semantic checks more strict
* Add mechanism to optimize well-known libc functions
* Support -dumpmachine
* Support -Wpointer-sign
* Stub support for -fasynchronous-unwind-tables, -funwind-tables, -frounding-math
* Support -fexcess-precision
* Support -nostdlib
* Support Makefile dependency generation (-M, -MD, ...)
* Support -fno-verbose-asm (-fverbose-asm is default)
* Improve diagnostics for disabled language extensions
* Support setting the mantissa with `__builtin_nan`
* Recognize, but reject, MSC asm statements, i.e. `__asm { ... }`
* Support attribute for custom printf-like format checks, e.g. `int my_printf(char const* fmt, ...) __attribute__((custom_format(1, "#0", "%": void, "d": int, "ld": long, "s": char const*));`
* Show an explanation for each warning switch when printing the help
* Improve PIC commandline options
* Support -f[no-]builtin

cparser 0.9.14 (2012-11-21)
---------------------------

* Adapt to libfirm-1.21.0
* Improved error recovery
* Improved firm graph generation (faster/smaller graphs)
* Implement U,u and u8 strings
* Preliminary preprocessor (we still use system cpp by default, as some macro
  expansion corner cases are still buggy and prevent us from compiling glibc
  headers)
* More gcc extensions: binary constants, __leaf__ attribute

cparser 0.9.13 (2011-12-07)
---------------------------

* Adapt to libfirm-1.20.0
* Implement --help
* More work on preprocessor (still not finished though)
* Refactoring work so others can reuse input, optimization order logic
* Columns in source positions (but external preprocessor doesn't preserve all spaces)
* Improvements to gnu builtins/attributes
* Bugfixes (we did alot of csmith testing)

cparser 0.9.12 (2011-03-15)
---------------------------

* Adapt to libfirm-1.19.0
* Introduce -mtarget (and -mtriple for llvm compatibility) for conventient
  cross-compilation
* Fix big-endian struct layouting
* Bugfixes

cparser 0.9.11 (2009-05-16)
---------------------------

* add missing NEWS entries
* fix crash when known C library functions had the wrong number of arguments

cparser 0.9.10 (2009-04-15)
---------------------------

* bugfixes
* adapt to libfirm-1.18.0

cparser 0.9.9 (2009-05-15)
--------------------------

* bugfixes
* extend and improve support for attributes
* adapat to latest libfirm

cparser 0.9.8 (2009-01-28)
--------------------------
* several bugfixes
* add/correct semantic checks
* improve error recovery
* support more GCC extensions
* add/improve/correct warnings

cparser 0.9.7 (2008-12-01)
--------------------------

* several bugfixes
* add/correct semantic checks
* improved error recovery
* support more GCC extensions
* support more GCC switches
* add a manpage

cparser 0.9.6 (2008-11-22)
--------------------------

* lots of bugfixes
* add/correct semantic checks
* more/improved warnings
* internal cleanups (introduce entity_t types)
* support more gnu extensions
* improved error recovery
* support more switches for gcc compatibility
* support for libc builtins

cparser 0.9.5 (2008-07-31)
--------------------------

* lots of bugfixes
* sync with latest libfirm
* improve error handling (more graceful continue in case of an error)
* compatibility fixes for old C stuff (=> SPECint2000 works now)
* improved commandline, more gcc compatibility flags
* support more gnu extensions
* parse all gnu extensions

cparser 0.9 (2008-02-08)
------------------------

* initial release
* cparser is able to bootstrap itself
