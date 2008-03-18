#ifndef TS
#define TS(x,str,val)
#endif

TS(HEADERNAME,              "header name", = 256)
TS(IDENTIFIER,              "identifier",)
TS(NUMBER,                  "number",)
TS(CHARACTER_CONSTANT,      "character constant",)
TS(WIDE_CHARACTER_CONSTANT, "character constant",)
TS(STRING_LITERAL,          "string literal",)
TS(WIDE_STRING_LITERAL,     "wide string literal",)
TS(PUNCTUATOR,              "punctuator",)
TS(NEWLINE,                 "newline", = '\n')

#define PUNCTUATOR(name,string,val)   T(_ALL,name,string,val)
#include "tokens_punctuator.inc"
#undef PUNCTUATOR

#define S(x)   T(_ALL,x,#x,)

S(if)
S(else)
S(elif)
S(endif)
S(ifdef)
S(ifndef)
S(include)
S(define)
S(undef)
S(line)
S(error)
S(pragma)

S(defined)
T(_ALL, va_args, "__VA_ARGS__",)
T(_MS, __COUNTER__, "__COUNTER__",)
T(_MS, __TIMESTAMP__, "__TIMESTAMP__",)

S(STDC)
S(ON)
S(OFF)
S(DEFAULT)
S(FP_CONTRACT)
S(FENV_ACCESS)
S(CX_LIMITED_RANGE)

#undef S
