#ifndef _UDP_H_

#include <arpa/inet.h>

typedef struct {
	int fd;
	struct addrinfo *addrinfo;
	struct addrinfo *addrinfos_head;
} udp_socket;

typedef struct sockaddr_storage udp_sender;


udp_socket udp_open_client_socket(const char *host, int short port);

udp_socket udp_open_server_socket(int port);

int udp_send(const udp_socket *sock, const void *buf, size_t nbytes);

int udp_recv(const udp_socket *sock, void *buf, size_t nbytes);

int udp_recvfrom(const udp_socket *sock, void *buf, size_t nbytes,
		udp_sender *sender);

void udp_sender_ip(const udp_sender *sender, char str[INET6_ADDRSTRLEN]);

void udp_close(udp_socket *sock);

#endif

