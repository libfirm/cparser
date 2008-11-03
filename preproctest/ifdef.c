before
#ifdef NOTDEFINED
inside
#endif
after

before
#ifdef NOTDEFINED
inside
#else
else
#endif
after

before
#ifndef NOTDEFINED
inside
#endif
after

before
#ifndef NOTDEFINED
inside
#else
else
#endif
after

#define JO1    1
#define JO2

#ifdef JO1
jo1 defined
#else
#endif

#ifdef JO2
jo2 defined
#endif

#ifndef JO1
jo1 not defined
#else
#endif

#ifndef JO2
jo2 not defined
#endif

#undef JO2

#ifdef JO1
jo1 defined
#else
#endif

#ifdef JO2
jo2 defined
#endif

#ifndef JO1
jo1 not defined
#else
#endif

#ifndef JO2
jo2 not defined
#endif
