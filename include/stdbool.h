#ifndef _STDBOOL_H
#define _STDBOOL_H

#ifndef __cplusplus

#define bool  _Bool
#define true  1
#define false 0

#elif defined(__GNUC__) && !defined(__STRICT_ANSI__)

#define _Bool bool
#define bool  bool
#define false false
#define true  true

#endif

#define __bool_true_false_are_defined 1

#endif
