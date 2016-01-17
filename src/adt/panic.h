/*
 * This file is part of cparser.
 * Copyright (C) 2012 Matthias Braun <matze@braunis.de>
 */
#ifndef ADT_ERROR_H
#define ADT_ERROR_H

void __attribute__((noreturn)) panic(char const *file, int line, char const *func, char const *msg, ...);

#define panic(...) panic(__FILE__, __LINE__, __func__, __VA_ARGS__)

#endif
