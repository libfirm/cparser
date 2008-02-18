/*
 * This file is part of cparser.
 * Copyright (C) 2007-2008 Matthias Braun <matze@braunis.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

/*
 * Project:     libFIRM
 * File name:   ir/adt/align.h
 * Purpose:     macros for alignment.
 * Author:      Markus Armbruster
 * Modified by:
 * Created:     1999 by getting from fiasco
 * CVS-ID:      $Id$
 * Copyright:   (c) 1995, 1996 Markus Armbruster
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _ALIGN_H
#define _ALIGN_H

#include <stddef.h>

/**
 * @file align.h
 */

/** A size handled efficiently by malloc(), at least 1K.  */
#define PREF_MALLOC_SIZE 2048


/** A wrapper around GNU C's __attribute__ */

/* According to the documentation, the attributes we are interested in
   work with 2.5, but we encountered trouble before 2.7.  */
#if defined (__GNUC__) && __GNUC__ >= 2 && __GNUC_MINOR__ >= 7
# define HAVE_ATTRIBUTE 1
# define ATTRIBUTE(attrs) __attribute__ (attrs)
#else
# define ATTRIBUTE(attrs)
#endif


/* Alignment */

/** A type that has most constrained alignment.  */
typedef union {
  long double d;
  void *p;
  long l;
} aligned_type ATTRIBUTE ((aligned));

/** Inquiring about the alignment of a type.  */
#ifdef __GNUC__
# define ALIGNOF(type) __alignof__ (type)
#else
# define ALIGNOF(type) offsetof (struct { char c; type d; }, d)
#endif

/** Maximal alignment required for any type.  */
#define MAX_ALIGN ALIGNOF (aligned_type)

#endif /* _ALIGN_H */
