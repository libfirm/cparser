/*
 * ISO C Standard:  7.16  Boolean type and values  <stdbool.h>
 */

#ifndef _STDBOOL_H
#define _STDBOOL_H

#ifndef __cplusplus

#ifdef __INTEL_COMPILER
#define bool    _Bool
#define true    ((_Bool)1)
#define false   ((_Bool)0)
#else

#define bool    unsigned
#define true    1
#define false   0

#endif
#endif /* __cplusplus */

#endif
