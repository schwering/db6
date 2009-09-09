#include "udp.h"
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <inttypes.h>

struct msg {
	char buf[4096 - sizeof(uint64_t)];
	uint64_t time;
};

int main(int argc, char **argv)
{
	udp_socket sock;
	udp_peer peer;
	struct msg msg;
	char peer_ip[16];

	sock = udp_open_server_socket(15345);
	while (udp_recvfrom(&sock, &msg, sizeof msg, &peer) == sizeof msg) {
#ifndef NDEBUG
		printf("recvd message to %s at %lld\n",
				udp_peer_ip(&peer, peer_ip),
				(long long unsigned)msg.time);
#endif
		int sentbytes = udp_sendto(&sock, &msg, sizeof msg, &peer);
		if (sentbytes != sizeof msg) {
			printf("only %d bytes sent\n", sentbytes);
			perror("reason");
			break;
		}
#ifndef NDEBUG
		printf("sent message to %s\n", udp_peer_ip(&peer, peer_ip));
#endif

		printf("recvd/sent with %s\n", udp_peer_ip(&peer, peer_ip));
		memset(&msg, 0, sizeof msg);
	}
	udp_close(&sock);
	return 0;
}

