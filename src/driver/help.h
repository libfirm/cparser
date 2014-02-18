/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef HELP_H
#define HELP_H

/**
 * Display information about a commandline option
 */
void put_help(const char *option, const char *explanation);

/**
 * Display a choice value for a multiple-choice option
 */
void put_choice(const char *choice, const char *explanation);

typedef enum {
	HELP_NONE          = 0,
	HELP_BASIC         = 1 << 0,
	HELP_PREPROCESSOR  = 1 << 1,
	HELP_PARSER        = 1 << 2,
	HELP_WARNINGS      = 1 << 3,
	HELP_OPTIMIZATION  = 1 << 4,
	HELP_CODEGEN       = 1 << 5,
	HELP_LINKER        = 1 << 6,
	HELP_LANGUAGETOOLS = 1 << 7,
	HELP_DEBUG         = 1 << 8,
	HELP_FIRM          = 1 << 9,

	HELP_ALL           = -1
} help_sections_t;

void help_usage(const char *argv0);

int action_help(const char *argv0);

extern help_sections_t help;

#endif
