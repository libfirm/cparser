#ifndef LANG_FEATURES_H
#define LANG_FEATURES_H

enum lang_features {
	_ANCIENT = 1,
	_ANSI    = 2,
	_C99     = 4,
	_GNUC    = 8,
	_MS      = 16,
	_ALL     = 0xFF
};

/* the current C mode/dialect */
extern unsigned int c_mode;

#endif
