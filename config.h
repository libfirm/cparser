#define INLINE inline

#ifdef __GNUC__
#define NORETURN __attribute__((noreturn))
#elseif defined(_MSC_VER)
#define NORETURN __declspec(noreturn)
#define __attribute__(x)
#else
#define NORETURN
#define __attribute__(x)
#endif
