static inline char *strchr(const char *s, int c)
{
	for ( ; *s != 0; ++s) {
		if (*s == c)
			return (char*) s;
	}
	return (char*) 0;
}

static inline char *strrchr(const char *s, int c)
{
	const char *result = (const char*) 0;

	for ( ; *s != 0; ++s) {
		if (*s == c)
			result = s;
	}
	return (char*) result;
}

static inline int strcmp(const char *s1, const char *s2)
{
	for( ; (*s1 | *s2) != 0; ++s1, ++s2) {
		if(*s1 != *s2)
			break;
	}
	return *s1 - *s2;
}

static inline __SIZE_TYPE__ strlen(const char *s)
{
	__SIZE_TYPE__ result = 0;
	for ( ; *s != 0; ++s)
		result++;
	return result;
}

static inline char *strcpy(char *dest, const char *source)
{
	char *d = dest;
	for ( ; *source != 0; ++d, ++source) {
		*d = *source;
	}
	return dest;
}
