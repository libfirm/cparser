/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */

/**
 * @file
 * @date    16.03.2007
 * @brief   Various utility functions that wrap compiler specific extensions
 * @author  Matthias Braun
 */
#ifndef _FIRM_UTIL_H_
#define _FIRM_UTIL_H_

#include <stdbool.h>

/**
 * Returns size of a static array. Warning: This returns invalid values for
 * dynamically allocated arrays.
 *
 * @param a    static array
 */
#define ARRAY_SIZE(a) (sizeof(a)/sizeof((a)[0]))

/**
 * Asserts that the constant expression x is not zero at compiletime. name has
 * to be a unique identifier.
 *
 * @note This uses the fact, that double case labels are not allowed.
 */
#define COMPILETIME_ASSERT(x, name) \
	static __attribute__((unused)) void compiletime_assert_##name (int h) { \
		switch(h) { case 0: case (x): {} } \
	}

/**
 * Indicates to the compiler that the value of x is very likely 1
 * @note Only use this in speed critical code and when you are sure x is often 1
 */
#define LIKELY(x)   __builtin_expect((x), 1)
/**
 * Indicates to the compiler that it's very likely that x is 0
 * @note Only use this in speed critical code and when you are sure x is often 0
 */
#define UNLIKELY(x) __builtin_expect((x), 0)

#ifdef __GNUC__
#define ENUMBF(type) __extension__ type
#else
#define ENUMBF(type) unsigned
#endif

#define endof(x) ((x) + ARRAY_SIZE(x))

#undef MAX
#undef MIN
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

static inline bool is_alpha(char const c)
{
	return ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z');
}

static inline bool is_digit(char const c)
{
	return '0' <= c && c <= '9';
}

#endif
