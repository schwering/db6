#include "udp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

static struct addrinfo *udp_get_client_addr_info(const char *host, int port)
{
	struct addrinfo hints, *addrinfo;
	int res;
	char portstr[8];

	sprintf(portstr, "%d", port);
	memset(&hints, 0, sizeof hints);
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_DGRAM;

	res = getaddrinfo(host, portstr, &hints, &addrinfo);
	return (res != 0) ? NULL : addrinfo;
}

static struct addrinfo *udp_get_server_addr_info(int port)
{
	struct addrinfo hints, *addrinfos_head;
	int res;
	char portstr[8];

	sprintf(portstr, "%d", port);
	memset(&hints, 0, sizeof hints);
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_DGRAM;
	hints.ai_flags = AI_PASSIVE;

	res = getaddrinfo(NULL, portstr, &hints, &addrinfos_head);
	return (res != 0) ? NULL : addrinfos_head;
}

udp_socket udp_open_client_socket(const char *host, int short port)
{
	struct addrinfo *ais, *ai;
	int fd;

	ais = udp_get_client_addr_info(host, port);
	if (ais == NULL) {
		fprintf(stderr, "addrinfo null\n");
		return (udp_socket){ .fd = -1, .addrinfo = NULL,
			.addrinfos_head = NULL};
	}
	for (ai = ais; ai != NULL; ai = ai->ai_next) {
		fd = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
		if (fd == -1) {
			perror("socket");
			continue;
		}
		break;
	}
	return (udp_socket){ .fd = fd, .addrinfo = ai, .addrinfos_head = ais };
}

udp_socket udp_open_server_socket(int port)
{
	struct addrinfo *ais, *ai;
	int fd;

	ais = udp_get_server_addr_info(port);
	if (ais == NULL) {
		fprintf(stderr, "addrinfo null\n");
		return (udp_socket){ .fd = -1, .addrinfo = NULL,
			.addrinfos_head = NULL};
	}
	for (ai = ais; ai != NULL; ai = ai->ai_next) {
		fd = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
		if (fd == -1) {
			perror("socket");
			continue;
		}
		if (bind(fd, ai->ai_addr, ai->ai_addrlen) == -1) {
			close(fd);
			perror("bind");
			continue;
		}
		break;
	}
	return (udp_socket){ .fd = fd, .addrinfo = ai, .addrinfos_head = ais };
}

int udp_send(const udp_socket *sock, const void *buf, size_t nbytes)
{
	return sendto(sock->fd, buf, nbytes, 0,
			sock->addrinfo->ai_addr, sock->addrinfo->ai_addrlen);
}

int udp_sendto(const udp_socket *sock, const void *buf, size_t nbytes,
		const udp_peer *sender)
{
	return sendto(sock->fd, buf, nbytes, 0, (struct sockaddr *)sender,
			sizeof *sender);
}

int udp_recv(const udp_socket *sock, void *buf, size_t nbytes)
{
	return recv(sock->fd, buf, nbytes, 0);
}

int udp_recvfrom(const udp_socket *sock, void *buf, size_t nbytes,
		udp_peer *sender)
{
	socklen_t addr_len;

	addr_len = sizeof *sender;
	return recvfrom(sock->fd, buf, nbytes, 0, (struct sockaddr *)sender,
			&addr_len);
}

char *udp_peer_ip(const udp_peer *sender, char str[INET6_ADDRSTRLEN])
{
	struct sockaddr *addr;
	void *in_addr;

	addr = (struct sockaddr *)sender;
	if (addr->sa_family == AF_INET)
		in_addr = &(((struct sockaddr_in *)addr)->sin_addr);
	else
		in_addr = &(((struct sockaddr_in6 *)addr)->sin6_addr);
	inet_ntop(sender->ss_family, in_addr, str, INET6_ADDRSTRLEN);
	return str;
}

void udp_close(udp_socket *sock)
{
	close(sock->fd);
	freeaddrinfo(sock->addrinfos_head);
	sock->fd = -1;
	sock->addrinfo = NULL;
	sock->addrinfos_head = NULL;
}

