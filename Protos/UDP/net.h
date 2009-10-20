/*
 * Copyright (C) 2005 Christoph Schwering
 * net.c and net.h offer a bunch of networking functions. Functions
 * open a server on a socket, wait for connections or actively connect
 * to such a server. A number of pairs of functions ease communication
 * between to hosts (exchanging strings and so on). Note that some
 * very general network functions are defined in util.c, util.h!
 */

#ifndef __NET_H__
#define __NET_H__

#include <stdlib.h>
#include <stdbool.h>

int aton(unsigned long *bin, const char *numbers_and_dots);
char *ntoa(unsigned long bin);

int open_server(int port);
void wait_for_connection(int lisock, int *sockptr, unsigned long *addrptr);
int connect_to_server(unsigned long addr, int port);
void disconnect(int sock);

bool is_connected(int sock);

ssize_t send_block(int sock, const void *buf, size_t len);
ssize_t recv_block(int sock, void *buf, size_t len);
ssize_t recv_callback(int sock, void *buf, size_t len,
		void (*func)(const void *buf, size_t off, size_t len));

#endif

