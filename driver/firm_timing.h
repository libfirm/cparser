/*
 * This file is part of cparser.
 * Copyright (C) 2012 Michael Beck <mm.beck@gmx.net>
 */

/**
 * @file
 * @brief timing for the Firm compiler
 */
#ifndef __FIRM_TIMING_H__
#define __FIRM_TIMING_H__

#include <stdio.h>
#include <libfirm/timing.h>

void timer_init(void);
void timer_register(ir_timer_t *timer, const char *description);
void timer_term(FILE *f);
void timer_push(ir_timer_t *timer);
void timer_pop(ir_timer_t *timer);
void timer_start(ir_timer_t *timer);
void timer_stop(ir_timer_t *timer);

#endif
