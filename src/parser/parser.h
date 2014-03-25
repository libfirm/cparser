/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef PARSER_H
#define PARSER_H

#include "ast/ast.h"
#include "ast/entity_t.h"
#include "ast/type.h"

/**
 * Initialize parser. Should be called once when the program starts
 */
void init_parser(void);

/**
 * Frees resources occupied by parser. Should be called once before the program
 * exits.
 */
void exit_parser(void);

/**
 * Start parsing a new compilation unit
 */
void start_parsing(void);

/**
 * Parse input. The source of the input is determined by the lexer module
 */
void parse(void);

/**
 * Finish parsing a complete compilation unit and return the AST.
 */
translation_unit_t *finish_parsing(void);

/**
 * record entities for the NAMESPACE_NORMAL, and produce error messages/warnings
 * for various problems that occur for multiple definitions
 */
entity_t *record_entity(entity_t *entity, bool is_definition);

/** set default elf visbility */
void set_default_visibility(elf_visibility_t visibility);

#endif
