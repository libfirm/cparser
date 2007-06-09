#ifndef TS
#define TS(x,str,val)
#endif

TS(IDENTIFIER,     "identifier", = 256)
TS(INTEGER,        "integer number",)
TS(STRING_LITERAL, "string literal",)

#define S(x)   T(x,#x,)
S(include)
S(define)
S(undef)
S(line)
S(error)
S(pragma)
S(if)
S(else)
S(elif)
S(endif)
S(ifdef)
S(ifndef)
#undef S

T(DOTDOTDOT,      "...",)

#define T_LAST_TOKEN  (T_DOTDOTDOT+1)

T(LPAREN,          "(", = '(')
T(RPAREN,          ")", = ')')
