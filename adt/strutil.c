#include "strutil.h"


bool streq_underscore(const char *const s1, const char *const s2)
{
	if (s2[0] == '_' && s2[1] == '_') {
		size_t len2 = strlen(s2);
		size_t len1 = strlen(s1);
		if (len1 == len2 - 4 && s2[len2 - 2] == '_' && s2[len2 - 1] == '_') {
			return strncmp(s1, s2 + 2, len2 - 4) == 0;
		}
	}

	return streq(s1, s2);
}
