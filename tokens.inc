#ifndef TS
#define TS(x,str,val)
#endif

/* These must go first. Some set explicit values. */
#define ALTERNATE(name, val)          T(_CXX, name, #name,  val)
#define PUNCTUATOR(name, string, val) T(_ALL, name, string, val)
#include "tokens_punctuator.inc"
#undef PUNCTUATOR
#undef ALTERNATE

TS(IDENTIFIER,              "identifier",)
TS(INTEGER,                 "integer number",)
TS(FLOATINGPOINT,           "floatingpoint number",)
TS(CHARACTER_CONSTANT,      "character constant",)
TS(WIDE_CHARACTER_CONSTANT, "wide character constant",)
TS(STRING_LITERAL,          "string literal",)

#define S(mode,x)   T(mode,x,#x,)
S(_ALL, auto)
S(_ALL, break)
S(_ALL, case)
S(_ALL, char)
S(_ALL, continue)
S(_ALL, default)
S(_ALL, do)
S(_ALL, double)
S(_ALL, else)
S(_ALL, enum)
S(_ALL, extern)
S(_ALL, float)
S(_ALL, for)
S(_ALL, goto)
S(_ALL, if)
S(_ALL, int)
S(_ALL, long)
S(_ALL, register)
S(_ALL, return)
S(_ALL, short)
S(_ALL, sizeof)
S(_ALL, static)
S(_ALL, struct)
S(_ALL, switch)
S(_ALL, typedef)
S(_ALL, union)
S(_ALL, unsigned)
S(_ALL, void)
S(_ALL, while)

/* C++ keywords */
#undef bool
S(_CXX, bool)
#define bool _Bool
S(_CXX, catch)
S(_CXX, class)
S(_CXX, const_cast)
S(_CXX, delete)
S(_CXX, dynamic_cast)
S(_CXX, explicit)
S(_CXX, export)
#undef false
S(_CXX, false)
#define false 0
S(_CXX, friend)
S(_CXX, mutable)
S(_CXX, namespace)
S(_CXX, new)
S(_CXX, operator)
S(_CXX, private)
S(_CXX, protected)
S(_CXX, public)
S(_CXX, reinterpret_cast)
S(_CXX, static_cast)
S(_CXX, template)
S(_CXX, this)
S(_CXX, throw)
#undef true
S(_CXX, true)
#define true 1
S(_CXX, try)
S(_CXX, typeid)
S(_CXX, typename)
S(_CXX, using)
S(_CXX, virtual)
S(_CXX, wchar_t)

S(_C99|_GNUC, _Bool)
S(_ALL, __thread)
S(_ALL, __extension__)
S(_ALL, __builtin_classify_type)
S(_ALL, __builtin_va_list)
S(_ALL, __builtin_va_arg)
S(_ALL, __builtin_va_copy)
S(_ALL, __builtin_offsetof)
S(_ALL, __builtin_constant_p)
S(_ALL, __builtin_types_compatible_p)
S(_ALL, __builtin_isgreater)
S(_ALL, __builtin_isgreaterequal)
S(_ALL, __builtin_isless)
S(_ALL, __builtin_islessequal)
S(_ALL, __builtin_islessgreater)
S(_ALL, __builtin_isunordered)
S(_ALL, __PRETTY_FUNCTION__)
S(_ALL, __label__)
S(_MS, __FUNCSIG__)
S(_MS, __FUNCDNAME__)
#undef S

/* needed on Windows */
#undef __w64
#undef __ptr32
#undef __ptr64

/* needed on MinGW */
#undef __int8
#undef __int16
#undef __int32
#undef __int64

/* needed on MinGW and Cygwin */
#undef __cdecl
#undef __stdcall
#undef __fastcall

/* needed on Darwin... */
#undef __signed
#undef __volatile
#undef __const
#undef __restrict

#define S(mode, x, val) T(mode, x, #x, val)
S(_C99,            __func__,)
S(_ALL,            __FUNCTION__,           = T___func__)
S(_ANSI|_C99|_CXX,   signed,               )
S(_ALL,            __signed,               = T_signed)
S(_ALL,            __signed__,             = T_signed)
S(_C99|_GNUC,       _Complex,              )
S(_ALL,            __complex__,            = T__Complex)
S(_ALL,            __complex,              = T__Complex)
S(_C99|_GNUC,       _Imaginary,            )
S(_ALL,            __real__,               )
S(_ALL,            __real,                 = T___real__)
S(_ALL,            __imag__,               )
S(_ALL,            __imag,                 = T___imag__)
S(_ALL,            __alignof__,            )
S(_ALL,            __alignof,              = T___alignof__)
S(_MS,              _alignof,              = T___alignof__)
S(_ANSI|_C99|_CXX,   const,                )
S(_ALL,            __const,                = T_const)
S(_ALL,            __const__,              = T_const)
S(_C99,              restrict,             )
S(_ALL,            __restrict__,           = T_restrict)
S(_ALL,            __restrict,             = T_restrict)
S(_MS,              _restrict,             = T_restrict)
S(_ALL,              asm,                  )
S(_ALL,            __asm__,                = T_asm)
S(_MS,              _asm,                  = T_asm)
S(_ALL,            __asm,                  = T_asm)
S(_ANSI|_C99|_CXX,   volatile,             )
S(_ALL,            __volatile,             = T_volatile)
S(_ALL,            __volatile__,           = T_volatile)
S(_C99|_CXX|_GNUC,   inline,               )
S(_ALL,            __inline,               = T_inline)
S(_ALL,            __inline__,             = T_inline)
S(_GNUC,             typeof,               )
S(_ALL,            __typeof,               = T_typeof)
S(_ALL,            __typeof__,             = T_typeof)
S(_ALL,            __attribute__,          )

S(_ALL,            __builtin_va_start,     )
S(_ALL,            __builtin_stdarg_start, = T___builtin_va_start)

S(_MS,              _near,                 )
S(_MS,             __near,                 = T__near)
S(_MS,              _far,                  )
S(_MS,             __far,                  = T__far)
S(_MS,               cdecl,                )
S(_MS,              _cdecl,                = T_cdecl)
S(_MS,             __cdecl,                = T_cdecl)
S(_MS,              _stdcall,              )
S(_MS,             __stdcall,              = T__stdcall)
S(_MS,              _fastcall,             )
S(_MS,             __fastcall,             = T__fastcall)
S(_MS,             __thiscall,             )
S(_MS,              _forceinline,          )
S(_MS,             __forceinline,          = T__forceinline)
S(_MS,             __unaligned,            )
S(_MS,              _assume,               )
S(_MS,             __assume,               = T__assume)
S(_MS,              _try,                  )
S(_MS,             __try,                  = T__try)
S(_MS,              _finally,              )
S(_MS,             __finally,              = T__finally)
S(_MS,              _leave,                )
S(_MS,             __leave,                = T__leave)
S(_MS,              _except,               )
S(_MS,             __except,               = T__except)
S(_MS,              _declspec,             )
S(_MS,             __declspec,             = T__declspec)
S(_MS,              _based,                )
S(_MS,             __based,                = T__based)
S(_MS,             __noop,                 )

S(_MS,             __ptr32,                )
S(_MS,             __ptr64,                )
S(_MS,             __sptr,                 )
S(_MS,             __uptr,                 )
S(_MS,              _w64,                  )
S(_MS,             __w64,                  = T__w64)

S(_MS,              _int8,                 )
S(_MS,             __int8,                 = T__int8)
S(_MS,              _int16,                )
S(_MS,             __int16,                = T__int16)
S(_MS,              _int32,                )
S(_MS,             __int32,                = T__int32)
S(_MS,              _int64,                )
S(_MS,             __int64,                = T__int64)
S(_MS,              _int128,               )
S(_MS,             __int128,               = T__int128)
#undef S

/* Redefine required macros for MinGW headers */
#if defined(__MINGW32__) || defined(__CYGWIN__)
#define __stdcall __attribute__((__stdcall__))
#define __fastcall __attribute__((__fastcall__))
#define __cdecl __attribute__((__cdecl__))
#endif
