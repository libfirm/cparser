#ifndef LANG_FEATURES_H
#define LANG_FEATURES_H

enum lang_features {
	_C89  = 1,
	_ANSI = 2,
	_C99  = 4,
	_GNUC = 8,
	_MS   = 16,
	_ALL  = 0xFF
};

/* the current C mode/dialect */
extern unsigned int c_mode;

/* the 'machine size', 16, 32 or 64 bit */
extern unsigned int machine_size;

/* true if the char type is signed */
extern bool char_is_signed;

/* true for strict language checking. */
extern bool strict_mode;

#endif
