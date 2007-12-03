/**
 * @file firm_timing.c -- timing for the Firm compiler
 *
 * (C) 2006  Michael Beck   beck@ipd.info.uni-karlsruhe.de
 *
 * $Id$
 */
#include <libcore/lc_timing.h>
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

static lc_timer_t *timers[TV_LAST];
static int timers_inited;

void timer_init(void) {
	int i;

	for (i = 0; i < TV_LAST; ++i) {
		timers[i] = lc_timer_register(tv_names[i], tv_desc[i]);
	}

	timers_inited = 1;
}

void timer_term(FILE *f) {
	int i;

	for (i = 0; i < TV_LAST; ++i) {
		unsigned long val = lc_timer_elapsed_msec(timers[i]);
		fprintf(f, "%-30s %8lu\n", tv_desc[i], val);
	}

	timers_inited = 0;
}

void timer_push(int timer) {
	if (timers_inited)
		lc_timer_push(timers[timer]);
}

void timer_pop(void) {
	if (timers_inited)
		lc_timer_pop();
}

void timer_start(int timer) {
	if (timers_inited)
		lc_timer_start(timers[timer]);
}

void timer_stop(int timer) {
	if (timers_inited)
		lc_timer_stop(timers[timer]);
}
