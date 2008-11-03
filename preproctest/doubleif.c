#define DEFINED

#ifndef NOT_DEFINE
	#ifdef DEFINED
		ok1
	#else
		bad
	#endif
#else
	bad
#endif

#ifndef HEADER_H
#define HEADER_H
bla
#ifndef HEADER_H
#define HEADER_H
#include "not_here.h"
nbla
#endif
#endif
