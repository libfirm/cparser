struct sockaddr {
	int bla;
};

struct sockaddr_at {
	int blo, blup;
};

typedef union bla {
	struct sockaddr *__restrict__     sockaddr_ptr;
	struct sockaddr_at *__restrict__  sockaddr_at_ptr;
} sockaddr_arg __attribute__((__transparent_union__));

void *t_recvfrom(sockaddr_arg arg) {
	return arg.sockaddr_at_ptr;
}

int main(void) {
	struct sockaddr at;
	union bla bla = { &at };
	int r = (t_recvfrom(bla) != &at);
	return r;
}
