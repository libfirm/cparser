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

#endif
