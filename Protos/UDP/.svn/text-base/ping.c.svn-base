#include "udp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

struct msg {
	char buf[4096 - sizeof(time_t)];
	time_t time;
};

int main(int argc, char **argv)
{
	udp_socket sock;
	struct msg msg;
	ssize_t trans;

	sock = udp_open_client_socket(argv[1], 15345);
	for (;;) {
		strcpy(msg.buf, "Hallo du kleiner Pupser");
		msg.time = time(NULL);
		trans = udp_send(&sock, &msg, sizeof msg);
		if (trans != sizeof msg) {
			printf("only %d bytes sent\n", trans);
			break;
		}
		printf("sent %s at %ld\n", msg.buf, msg.time);
		memset(&msg, 0, sizeof msg);
		trans = udp_recv(&sock, &msg, sizeof msg);
		if (trans != sizeof msg) {
			printf("only %d bytes recv\n", trans);
			break;
		}
		printf("recvd %s at %ld\n", msg.buf, msg.time);
	}
	udp_close(&sock);
	return 0;
}

