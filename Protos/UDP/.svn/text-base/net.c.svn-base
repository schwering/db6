/*
 * Copyright (C) 2005 Christoph Schwering
 * net.c and net.h offer a bunch of networking functions. Functions
 * open a server on a socket, wait for connections or actively connect
 * to such a server. A number of pairs of functions ease communication
 * between to hosts (exchanging strings and so on). Note that some
 * very general network functions are defined in util.c, util.h!
 */

#include "net.h"
#include <arpa/inet.h>
#include <inttypes.h>
#include <netdb.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>

int aton(unsigned long *bin, const char *numbers_and_dots)
{
	return inet_aton(numbers_and_dots, (struct in_addr *)bin);
}

char *ntoa(unsigned long bin)
{
	return inet_ntoa(*(struct in_addr *)&bin);
}

int open_server(int port)
{
	int lisock;
	struct sockaddr_in addr;
	
	lisock = socket(AF_INET, SOCK_STREAM, 0);
	if (lisock == -1)
		return -1;
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = htonl(INADDR_ANY);
	addr.sin_port = htons(port);
	if (bind(lisock, (struct sockaddr *)&addr, sizeof addr) == -1)
		return -1;
	listen(lisock, 5);
	return lisock;
}

void wait_for_connection(int lisock, int *sockptr, unsigned long *addrptr)
{
	socklen_t clientsize;
	struct sockaddr_in client;
	
	clientsize = sizeof(struct sockaddr_in);
	*sockptr = accept(lisock, (struct sockaddr *)&client, &clientsize);
	*addrptr = client.sin_addr.s_addr;
}

#if 0
#include <pthread.h>
static void wait_for_connection_thread(void *args[3]);

void wait_for_connection(int lisock, int timeout,
		int *sock, unsigned long *addr)
{
	pthread_t thread;
	void *args[] = { (void *)&lisock, (void *)sockptr, (void *)addrptr };
	
	*sockptr = -1;
	pthread_create(&thread, NULL, (void *)wait_for_connection_thread, args);
	if (timeout == NO_TIMEOUT) {
		pthread_join(thread, NULL);
	} else {
		int i;
		for (i = 0; i < timeout && *sockptr == -1; i++)
			sleep(1);
	}
	pthread_cancel(thread);
}

static void wait_for_connection_thread(void *args[3])
{
	int *lisockptr, *sockptr, clientsize;
	unsigned long *addrptr;
	struct sockaddr_in client;
	
	lisockptr = args[0];
	sockptr = args[1];
	addrptr = args[2];
	
	clientsize = sizeof(struct sockaddr_in);
	*sockptr = accept(*lisock, (struct sockaddr *)&client, &clientsize);
	*addrptr = client.sin_addr.s_addr;
}
#endif

int connect_to_server(unsigned long addr, int port)
{
	int sock;
	struct sockaddr_in addr_in;
	
	sock = socket(AF_INET, SOCK_STREAM, 0);
	if (sock == -1)
		return -1;
	addr_in.sin_family = AF_INET;
	addr_in.sin_addr.s_addr = addr;
	addr_in.sin_port = htons(port);
	
	return (connect(sock, (struct sockaddr *)&addr_in, 
	    sizeof(struct sockaddr_in)) != -1) ? sock : -1;
}

void disconnect(int sock)
{
	close(sock);
}

bool is_connected(int sock)
{
	struct sockaddr dummyaddr;
	socklen_t dummysize;
	
	memset(&dummyaddr, 0, sizeof(struct sockaddr));
	dummysize = sizeof(struct sockaddr);
	return getpeername(sock, &dummyaddr, &dummysize) == 0;
}

ssize_t send_block(int sock, const void *buf, size_t len)
{
	return send(sock, buf, len, 0);
}

ssize_t recv_block(int sock, void *buf, size_t len)
{
	return recv(sock, buf, len, MSG_WAITALL);
}

ssize_t recv_callback(int sock, void *buf, size_t len,
		void (*func)(const void *buf, size_t off, size_t len))
{
	size_t off, done;
	ssize_t ret;

	done = 0;
	do {
		off = done;
		ret = recv(sock, buf + off, len - off, 0);
		if (ret == -1)
			return done;
		done += ret;
		if (func)
			func(buf, off, done);
	} while (done < len);
	return done;
}

