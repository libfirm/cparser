
#define FOO1    bar
# define FOO2   bar
#	define FOO3 bar
# define FOO4/*  */bar
#/*blup*/	/*bla*/define FOO5 bar
#/*blup
  */define FOO6 bar
#//foobar
define FOO7 bar
#
define FOO8 bar

FOO1
FOO2
FOO3
FOO4
FOO5
FOO6
FOO7
FOO8

#define foo foo1
#/*inc*/include/* haha
*/ "preproctest/simpleinc.h"
#undef foo
#define foo foo2
#/*inc*/include/* haha*/<preproctest/simpleinc.h>
