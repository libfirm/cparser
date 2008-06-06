typedef struct atomic_type_properties_t atomic_type_properties_t;
struct atomic_type_properties_t {
	unsigned   size;              /**< type size in bytes */
};

enum atomic_types {
	ATOMIC_TYPE_VOID = 0,
	ATOMIC_TYPE_CHAR,
	ATOMIC_TYPE_LAST = ATOMIC_TYPE_CHAR
};

static atomic_type_properties_t atomic_type_properties[ATOMIC_TYPE_LAST+1] = {
	[ATOMIC_TYPE_CHAR] = {
		.size      = 1,
	},
	[ATOMIC_TYPE_VOID] = {
		.size      = 0,
	},
};

int main(void)
{
	return atomic_type_properties[ATOMIC_TYPE_CHAR].size != 1;
}
