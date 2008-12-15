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
#ifndef LEXER_H
#define LEXER_H

#include "symbol_table_t.h"
#include "token_t.h"

extern token_t lexer_token;
extern bool    allow_dollar_in_symbol;

void lexer_next_token(void);

/* for debugging */
void lexer_next_preprocessing_token(void);

void init_lexer(void);
void exit_lexer(void);

void select_input_encoding(char const* encoding);

void lexer_open_stream(FILE *stream, const char *input_name);
void lexer_open_buffer(const char *buffer, size_t len, const char *input_name);

string_t      concat_strings(           const string_t      *s1, const string_t      *s2);
wide_string_t concat_string_wide_string(const string_t      *s1, const wide_string_t *s2);
wide_string_t concat_wide_strings(      const wide_string_t *s1, const wide_string_t *s2);
wide_string_t concat_wide_string_string(const wide_string_t *s1, const string_t      *s2);

#endif
