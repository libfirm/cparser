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

#define lengthof(x) (sizeof(x) / sizeof(*(x)))

#define endof(x) ((x) + lengthof(x))

#endif
