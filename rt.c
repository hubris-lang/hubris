#include <stdio.h>

extern void _hubris_main();

int main() {
	_hubris_main();
	return 0;
}

void hello_world() {
	printf("hello world");
}
