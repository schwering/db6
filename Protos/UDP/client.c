#include <stdio.h>
#include <time.h>
#include "net.h"

#define BUFSIZE 4096
//#define SERVER	"127.0.0.1"
#define SERVER	"192.168.0.1"
#define PORT	23285

static void print(const void *buf, size_t off, size_t len)
{
	printf("off = %u, len = %u\n", (unsigned)off, (unsigned)len);
}

int main(int argc, char *argv[])
{
	unsigned long int addr;
	int sock, i;
	char buf[BUFSIZE];

	if (argc >= 2)
		aton(&addr, argv[1]);
	else
		aton(&addr, SERVER);

	sock = connect_to_server(addr, PORT);
	if (sock == -1) {
		printf("connecting failed\n");
		return 1;
	}
	printf("communicating\n");
	for (i = 0; i < 10; i++) {
		clock_t clk;
		time_t tm;
		ssize_t size;
		int j;

		clk = clock();
		tm  = time(NULL);

		for (j = 0; j < 10; j++) {
			size = send_block(sock, buf, sizeof(buf));
			if (size != sizeof(buf))
				printf("sent %d bytes\n", (int)size);
			size = recv_callback(sock, buf, sizeof(buf), NULL);
			if (size != sizeof(buf))
				printf("received %d bytes\n", (int)size);
		}

		clk = clock() - clk;
		tm  = time(NULL) - tm;
		printf("%u ticks\n", (unsigned int)clk);
		printf("%u secs\n", (unsigned int)tm);
	}
	disconnect(sock);
	return 0;
}

