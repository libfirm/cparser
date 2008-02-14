typedef enum {
	TYPE_QUALIFIER_NONE     = 0,
	TYPE_QUALIFIER_CONST    = 1 << 0,
	TYPE_QUALIFIER_RESTRICT = 1 << 1,
	TYPE_QUALIFIER_VOLATILE = 1 << 2,
} type_qualifier_t;

typedef unsigned int type_qualifiers_t;

typedef struct source_position_t source_position_t;
struct source_position_t {
	const char *input_name;
	unsigned    linenr;
};

typedef struct ir_type ir_type;

typedef enum {
	TYPE_INVALID,
	TYPE_ERROR,
	TYPE_ATOMIC,
} type_kind_t;

struct type_base_t {
	type_kind_t       kind;
	type_qualifiers_t qualifiers;
	source_position_t source_position;

	ir_type          *firm_type;
};
typedef struct type_base_t type_base_t;

#define NULL (void*)0

int main(void)
{
	static const type_base_t error = { TYPE_ERROR, TYPE_QUALIFIER_NONE, { NULL, 0 }, NULL };
	return 0;
}
