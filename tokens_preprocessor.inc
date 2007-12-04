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

#undef S

#define TP_LAST_TOKEN  (TP_pragma + 1)
