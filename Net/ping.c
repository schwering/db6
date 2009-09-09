#include "udp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <inttypes.h>
#include <sys/time.h>

struct msg {
	char buf[4096 - sizeof(uint64_t)];
	uint64_t time;
};

inline uint64_t now_micros(void)
{
	struct timeval tv;

	if (gettimeofday(&tv, NULL))
		return 0;
	return 10E6 * tv.tv_sec + tv.tv_usec;
}


int main(int argc, char **argv)
{
	udp_socket sock;
	struct msg msg;
	ssize_t trans;
	uint64_t diff;

	sock = udp_open_client_socket(argv[1], 15345);
	for (;;) {
		strcpy(msg.buf, "Hallo du kleiner Pupser");
		msg.time = now_micros();
		trans = udp_send(&sock, &msg, sizeof msg);
		if (trans != sizeof msg) {
			printf("only %d bytes sent\n", (int)trans);
			break;
		}
#ifndef NDEBUG
		printf("sent message to %s at %lld\n", argv[1],
				(long long unsigned)msg.time);
#endif

		memset(&msg, 0, sizeof msg);
		trans = udp_recv(&sock, &msg, sizeof msg);
		if (trans != sizeof msg) {
			printf("only %d bytes recv\n", (int)trans);
			break;
		}
#ifndef NDEBUG
		printf("recvd message to %s at %lld\n", argv[1],
				(long long unsigned)msg.time);
#endif

		diff = now_micros() - msg.time;
		printf("recvd after %lld ns\n", 
				(long long unsigned)(diff * 1000));
		sleep(1);
	}
	udp_close(&sock);
	printf("ended\n");
	return 0;
}

