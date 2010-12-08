include ../Makefile.inc

all: server_debug

server_debug:
	$(GPRBUILD) -p -Pserver -XBUILD=debug -XKIND=static
	#$(GPRBUILD) -p -Pserver -XBUILD=debug -XKIND=relocatable

server_release:
	$(GPRBUILD) -p -Pserver -XBUILD=release -XKIND=static
	#$(GPRBUILD) -p -Pserver -XBUILD=release -XKIND=relocatable

clean_server:
	$(GPRCLEAN) -f -q server.gpr -XBUILD=debug -XKIND=static || true
	#$(GPRCLEAN) -f -q server.gpr -XBUILD=debug -XKIND=relocatable || true
	$(GPRCLEAN) -f -q server.gpr -XBUILD=release -XKIND=static || true
	#$(GPRCLEAN) -f -q server.gpr -XBUILD=release -XKIND=relocatable || true

clean: clean_server

