CFLAGS=-DNDEBUG -Wall

all: ping pong

ping: ping.c udp.h udp.c
	$(CC) $(CFLAGS) udp.c ping.c -o ping

pong: pong.c udp.h udp.c
	$(CC) $(CFLAGS) udp.c pong.c -o pong

clean:
	rm -f ping.o ping pong.o pong udp.o

