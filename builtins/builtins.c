static inline char *strchr(const char *s, int c)
{
	do {
		if (*s == c)
			return (char*) s;
	} while (*s++ != '\0');
	return (char*) 0;
}

static inline char *strrchr(const char *s, int c)
{
	const char *result = (const char*) 0;

	do {
		if (*s == c)
			result = s;
	} while (*s++ != '\0');
	return (char*) result;
}

static inline int strcmp(const char *s1, const char *s2)
{
	for( ; *s1 != 0; ++s1, ++s2) {
		if(*s1 != *s2)
			break;
	}
	return (unsigned char)*s1 - (unsigned char)*s2;
}

/*
static inline __SIZE_TYPE__ strlen(const char *s)
{
	__SIZE_TYPE__ result = 0;
	for ( ; *s != 0; ++s)
		result++;
	return result;
}
*/

static inline __SIZE_TYPE__ strlen(const char *str)
{
	const char          *char_ptr;
	const unsigned long *long_ptr;
	unsigned long int longword, magic_bits, himagic, lomagic;
	int abort(void);

	for (char_ptr = str; ((unsigned long int) char_ptr & (sizeof(longword)-1)) != 0; ++char_ptr) {
		if (*char_ptr == '\0')
			return char_ptr - str;
	}

	long_ptr = (unsigned long int*) char_ptr;
	magic_bits = 0x7efefeffL;
	himagic    = 0x80808080L;
	lomagic    = 0x01010101L;
	if (sizeof(longword) > 4)
		abort();

	for(;;) {
		longword = *long_ptr++;
		if (((longword - lomagic) & himagic) != 0) {
			const char *cp = (const char*) (long_ptr - 1);
			if (cp[0] == 0)
				return cp-str;
			if (cp[1] == 0)
				return cp-str+1;
			if (cp[2] == 0)
				return cp-str+2;
			if (cp[3] == 0)
				return cp-str+3;
		}
	}
}

static inline char *strcpy(char *dest, const char *source)
{
	char *d = dest;
	while ((*d++ = *source++) != '\0') {}
	return dest;
}
