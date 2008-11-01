// works with gcc 3.4.4 and icc 10.1
typedef void(*__cdecl PIMAGE_TLS_CALLBACK)();
typedef void(__cdecl* PIMAGE_TLS_CALLBACK2)();

// works with gcc 3.4.4
typedef void(__attribute__((__stdcall__)) *PIMAGE_TLS_CALLBACK3)();
typedef void(*__attribute__((__stdcall__)) PIMAGE_TLS_CALLBACK4)();

int main(void)
{
	return 0;
}
