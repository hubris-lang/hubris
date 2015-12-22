#include <stdio.h>

/* This is a temporary hack, used to link
   together the Rust runtime with object
	 files spit out by LLVM */
extern int _hubris_main();

int main() {
	_hubris_main();
	return 0;
}

void hello_world(int a) {
	printf("hello world");
}
