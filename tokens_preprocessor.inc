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

S(STDC)
S(ON)
S(OFF)
S(DEFAULT)
S(FP_CONTRACT)
S(FENV_ACCESS)
S(CX_LIMITED_RANGE)

#undef S

#define TP_LAST_TOKEN  (TP_CX_LIMITED_RANGE + 1)
