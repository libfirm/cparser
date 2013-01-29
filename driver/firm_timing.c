/*
 * This file is part of cparser.
 * Copyright (C) 2012 Michael Beck <mm.beck@gmx.net>
 */

/**
 * @file
 * @brief timing for the Firm compiler
 */
#include "firm_timing.h"

#include <libfirm/adt/xmalloc.h>

static int timers_inited;

typedef struct timer_info_t {
	struct timer_info_t *next;
	char                *description;
	ir_timer_t          *timer;
} timer_info_t;

static timer_info_t *infos;
static timer_info_t *last_info;

void timer_register(ir_timer_t *timer, const char *description)
{
	timer_info_t *info = XMALLOCZ(timer_info_t);

	info->description = xstrdup(description);
	info->timer       = timer;

	if (last_info != NULL) {
		last_info->next = info;
	} else {
		infos = info;
	}
	last_info = info;
}

void timer_init(void)
{
	timers_inited = 1;
}

void timer_term(FILE *f)
{
	timer_info_t *info;
	timer_info_t *next;

	for (info = infos; info != NULL; info = next) {
		ir_timer_t *timer = info->timer;
		if (f != NULL) {
			double      val         = (double)ir_timer_elapsed_usec(timer) / 1000.0;
			const char *description = info->description;
			fprintf(f, "%-60s %10.3f msec\n", description, val);
		}

		ir_timer_free(timer);
		free(info->description);
		next = info->next;
		free(info);
	}
	infos = NULL;
	last_info = NULL;

	timers_inited = 0;
}

void timer_push(ir_timer_t *timer)
{
	if (timers_inited)
		ir_timer_push(timer);
}

void timer_pop(ir_timer_t *timer)
{
	if (timers_inited)
		ir_timer_pop(timer);
}

void timer_start(ir_timer_t *timer)
{
	if (timers_inited)
		ir_timer_start(timer);
}

void timer_stop(ir_timer_t *timer)
{
	if (timers_inited)
		ir_timer_stop(timer);
}
