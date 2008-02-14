#ifndef __FIRM_TIMING_H__
#define __FIRM_TIMING_H__
/**
 * @file firm_timing.h -- timing for the Firm compiler
 *
 * (C) 2006  Michael Beck   beck@ipd.info.uni-karlsruhe.de
 *
 * $Id$
 */

#include <stdio.h>

enum timings {
#define DEFTIMEVAR(x, y, z)	x,
#include "firm_timing.def"
	TV_LAST
#undef DEFTIMEVAR
};

void timer_init(void);
void timer_term(FILE *f);
void timer_push(int timer);
void timer_pop(void);
void timer_start(int timer);
void timer_stop(int timer);

#endif /* __FIRM_TIMING_H__ */
