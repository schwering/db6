#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <time.h>

int main(void) {
	printf("sizeof(long) = %u\n", (unsigned)sizeof(long));
	printf("sizeof(off_t) = %u\n", (unsigned)sizeof(off_t));
	/*printf("sizeof(off64_t) = %u\n", (unsigned)sizeof(off64_t));*/
	printf("sizeof(size_t) = %u\n", (unsigned)sizeof(size_t));
	printf("sizeof(ssize_t) = %u\n", (unsigned)sizeof(ssize_t));
	printf("sizeof(clock_t) = %u\n", (unsigned)sizeof(clock_t));
	printf("CLOCKS_PER_SEC = %d\n", CLOCKS_PER_SEC);
	return 0;
}

