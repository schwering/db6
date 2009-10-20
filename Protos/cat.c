#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
	char file[512];
	int from, len, ret;
	void *ptr;
	FILE *fp;

	sscanf(argv[1], "%s", file);
	sscanf(argv[2], "%d", &from);
	sscanf(argv[3], "%d", &len);

	fp = fopen(file, "r");
	fseek(fp, 4096 * from, SEEK_SET);
	ptr = malloc(len);
	ret = fread(ptr, sizeof(char), len, fp);
	fclose(fp);
	if (ret != len) {
		fprintf(stderr, "Fehler %d != %d", ret, len);
		return 1;
	}
	fwrite(ptr, sizeof(char), len, stdout);
	fflush(stdout);
	return 0;
}

