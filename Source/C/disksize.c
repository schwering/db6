#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <errno.h>

#define OPEN_RO_FLAGS	0
#define FILE_MODE	(S_IRUSR | S_IWUSR)

#define KB	(1024)
#define MB	(1024 * KB)
#define GB	(1024 * MB)

int main(int argc, char **argv)
{
	const char *disk;
	int fd;
	off_t pos;

	if (argc >= 2)
		disk = argv[1];
	else
		disk = "/dev/sda4";

	fd = open(disk, OPEN_RO_FLAGS, FILE_MODE);
	if (fd == -1) {
		printf("error opening %s: %s\n", disk, strerror(errno));
		return 0;
	}

	pos = lseek(fd, 0, SEEK_END);
	printf("%lld bytes\n", (long long)pos);
	printf("%lld Kbytes\n", (long long)(pos / KB));
	printf("%lld Mbytes\n", (long long)(pos / MB));
	printf("%lld Gbytes\n", (long long)(pos / GB));
	close(fd);
	return 0;
}

