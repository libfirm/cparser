/**
 * @file firm_timing.c -- timing for the Firm compiler
 *
 * (C) 2006-2009  Michael Beck   beck@ipd.info.uni-karlsruhe.de
 *
 * $Id$
 */
#include <libfirm/timing.h>
#include "firm_timing.h"

static const char *tv_names[] = {
#define DEFTIMEVAR(x, y, z)	y,
#include "firm_timing.def"
	NULL
#undef DEFTIMEVAR
};

static const char *tv_desc[] = {
#define DEFTIMEVAR(x, y, z)	z,
#include "firm_timing.def"
	NULL
#undef DEFTIMEVAR
};

static ir_timer_t *timers[TV_LAST];
static int timers_inited;

void timer_init(void) {
	int i;

	for (i = 0; i < TV_LAST; ++i) {
		timers[i] = ir_timer_register(tv_names[i], tv_desc[i]);
	}

	timers_inited = 1;
}

void timer_term(FILE *f) {
	int i;

	for (i = 0; i < TV_LAST; ++i) {
		double val = (double)ir_timer_elapsed_usec(timers[i]) / 1000.0;
		fprintf(f, "%-30s %8.3f msec\n", tv_desc[i], val);
	}

	timers_inited = 0;
}

void timer_push(int timer) {
	if (timers_inited)
		ir_timer_push(timers[timer]);
}

void timer_pop(void) {
	if (timers_inited)
		ir_timer_pop();
}

void timer_start(int timer) {
	if (timers_inited)
		ir_timer_start(timers[timer]);
}

void timer_stop(int timer) {
	if (timers_inited)
		ir_timer_stop(timers[timer]);
}
