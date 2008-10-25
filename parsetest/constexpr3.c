typedef struct {
	int trTime;
	float vec[3];
}  trajectory_t;

typedef struct {
	trajectory_t pos;
} entityState_t;

typedef struct {
	char    *name;
	int     offset;
	int     bits;       // 0 = float
} netField_t;

#define NETF(x) #x,(unsigned)&((entityState_t*)0)->x

netField_t entityStateFields[] =
{
	{ NETF(pos.vec[0]), 32 },
};

int main(void) {
	return 0;
}
