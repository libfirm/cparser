#ifndef _FIRM_HASH_STRING_H_
#define _FIRM_HASH_STRING_H_

#define _FIRM_FNV_OFFSET_BASIS 2166136261U
#define _FIRM_FNV_FNV_PRIME 16777619U

static INLINE __attribute__((pure))
unsigned hash_string(const char* str)
{
	const unsigned char *p;
	unsigned hash = _FIRM_FNV_OFFSET_BASIS;

	for(p = (const unsigned char*) str; *p != 0; ++p) {
		hash *= _FIRM_FNV_FNV_PRIME;
		hash ^= *p;
	}

	return hash;
}

static INLINE __attribute__((pure))
unsigned hash_string_size(const char* str, size_t size)
{
	size_t i;
	unsigned hash = _FIRM_FNV_OFFSET_BASIS;

	for(i = 0; i < size; ++i) {
		hash *= _FIRM_FNV_FNV_PRIME;
		hash ^= str[i];
	}

	return hash;
}

#endif
