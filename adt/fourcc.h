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
 * File name:   ir/adt/fourcc.h
 * Purpose:     define the famous infame FOURCC macro.
 * Author:
 * Modified by:
 * Created:     02.01.2004
 * CVS-ID:      $Id$
 * Copyright:   (C) 2004 University of Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _FOURCC_H
#define _FOURCC_H

/** define a readable fourcc code */
#define FOURCC(a,b,c,d)         ((a) | ((b) << 8) | ((c) << 16) | ((d) << 24))

#endif /* _FOURCC_H */
