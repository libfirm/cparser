// Aus CygWin's /usr/include/asm/byteorder.h zusammenkopiert
// Es gibt keine weitere Definition von __constant_ntohl

typedef unsigned long uint32_t;

extern __inline__ uint32_t	__ntohl(uint32_t);
extern __inline__ uint32_t	__constant_ntohl(uint32_t);

extern __inline__ uint32_t
__ntohl(uint32_t x)
{
	__asm__("xchgb %b0,%h0\n\t"	/* swap lower bytes	*/
		"rorl $16,%0\n\t"	/* swap words		*/
		"xchgb %b0,%h0"		/* swap higher bytes	*/
		:"=q" (x)
		: "0" (x));
	return x;
}

#define __constant_ntohl(x) \
	((uint32_t)((((uint32_t)(x) & 0x000000ffU) << 24) | \
		   (((uint32_t)(x) & 0x0000ff00U) <<  8) | \
		   (((uint32_t)(x) & 0x00ff0000U) >>  8) | \
		   (((uint32_t)(x) & 0xff000000U) >> 24)))

int main(void)
{
	return 0;
}
