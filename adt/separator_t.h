/*
 * This file is part of cparser.
 * Copyright (C) 2012 Christoph Mallon <christoph.mallon@gmx.de>
 */
#ifndef SEPARATOR_H
#define SEPARATOR_H

#include <stdbool.h>

typedef struct separator_t separator_t;
struct separator_t
{
	char const *first; /**< Returned on the first call to sep_next(). */
	char const *rest;  /**< Returned on all further calls to sep_next(). */
};

/**
 * Returns first on the first call for s and rest on all further calls.
 */
static inline char const* sep_next(separator_t* const s)
{
	char const *const cur = s->first;
	s->first = s->rest;
	return cur;
}

/**
 * Returns whether sep_next() has not been called for s.
 */
static inline bool sep_at_first(separator_t const* const s)
{
	return s->first != s->rest;
}

#endif
