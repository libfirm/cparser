/*
 * This file is part of cparser.
 * Copyright (C) 2013 Matthias Braun <matze@braunis.de>
 */
#ifndef ACTIONS_H
#define ACTIONS_H

extern const char *print_file_name_file;

int action_version(const char *argv0);

int action_version_short(const char *argv0);

int action_print_file_name(const char *argv0);

#endif
