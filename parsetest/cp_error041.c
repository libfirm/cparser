enum pr_opcode_e {
	blub
};

struct statement_s {
	enum pr_opcode_e op:16;
};

int main(void)
{
	return sizeof(struct statement_s) == 0;
}
