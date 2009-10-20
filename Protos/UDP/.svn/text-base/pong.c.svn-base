#include "udp.h"
#include <stdio.h>
#include <string.h>

struct msg {
	char buf[4096 - sizeof(time_t)];
	time_t time;
};

int main(int argc, char **argv)
{
	udp_socket sock;
	struct msg msg;

	sock = udp_open_server_socket(15345);
	while (udp_recv(&sock, &msg, sizeof msg) == sizeof msg) {
		printf("recvd %s at %ld\n", msg.buf, msg.time);
		int sentbytes = udp_send(&sock, &msg, sizeof msg);
		if (sentbytes != sizeof msg) {
			printf("only %d bytes sent\n", sentbytes);
			break;
		}
		printf("sent %s at %ld\n", msg.buf, msg.time);
		memset(&msg, 0, sizeof msg);
	}
	udp_close(&sock);
	return 0;
}

