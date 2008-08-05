typedef enum {
	blub
} pr_opcode_e;

typedef struct statement_s {
	pr_opcode_e		op:16;
} dstatement_t;

dstatement_t *ptr;

int main(void)
{
	ptr++;
	return 0;
}
