#ifndef _CPARSER_LIMITS_H____
#define _CPARSER_LIMITS_H____

/* for gcc compatibility */
#define _GCC_LIMITS_H_

#include_next <limits.h>

/* note: we do not define MB_LEN_MAX */
#define CHAR_BIT   __CHAR_BIT__
#define SCHAR_MIN  (-SCHAR_MAX - 1)
#define SCHAR_MAX  __SCHAR_MAX__
#if __SCHAR_MAX__ == __INT_MAX__
# define UCHAR_MAX (SCHAR_MAX * 2U + 1U)
#else
# define UCHAR_MAX (SCHAR_MAX * 2 + 1)
#endif
#ifdef __CHAR_UNSIGNED__
# if __SCHAR_MAX__ == __INT_MAX__
#  define CHAR_MIN 0U
# else
#  define CHAR_MIN 0
# endif
# define CHAR_MAX UCHAR_MAX
#else
# define CHAR_MIN SCHAR_MIN
# define CHAR_MAX SCHAR_MAX
#endif

#define SHRT_MIN (-SHRT_MAX - 1)
#define SHRT_MAX __SHRT_MAX__
#if __SHRT_MAX__ == __INT_MAX__
# define USHRT_MAX (SHRT_MAX * 2U + 1U)
#else
# define USHRT_MAX (SHRT_MAX * 2 + 1)
#endif
#define INT_MIN    (-INT_MAX - 1)
#define INT_MAX    __INT_MAX__
#define UINT_MAX   (INT_MAX * 2U + 1U)
#define LONG_MIN   (-LONG_MAX - 1L)
#define LONG_MAX   __LONG_MAX__
#define ULONG_MAX  (LONG_MAX * 2UL + 1UL)
#define LLONG_MIN  (-LLONG_MAX - 1LL)
#define LLONG_MAX  __LONG_LONG_MAX__
#define ULLONG_MAX (LLONG_MAX * 2ULL + 1ULL)

#else

#include_next <limits.h>

#endif
