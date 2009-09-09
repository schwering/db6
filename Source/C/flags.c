#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <stdio.h>

int main(int argc, char **argv)
{
#if defined(O_NOATIME) && defined(O_DIRECT)
	printf("(O_RDWR | O_DIRECT | O_NOATIME)\n");
#elif !defined(O_NOATIME) && defined(O_DIRECT)
	printf("(O_RDWR | O_DIRECT), no O_NOATIME\n");
#elif defined(O_NOATIME) && !defined(O_DIRECT)
	printf("(O_RDWR | O_NOATIME), no O_DIRECT\n");
#else 
	printf("(O_RDWR), no O_NOATIME, O_DIRECT\n");
#endif
	return 0;
}

