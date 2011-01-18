include ../Makefile.inc

all: debug

debug: server_debug unit_tests_debug
release: server_release unit_tests_release

libdb_debug:
	$(GPRBUILD) -p -Plibdb -XBUILD=debug -XKIND=static
	$(GPRBUILD) -p -Plibdb -XBUILD=debug -XKIND=relocatable

libdb_release:
	$(GPRBUILD) -p -Plibdb -XBUILD=release -XKIND=static
	$(GPRBUILD) -p -Plibdb -XBUILD=release -XKIND=relocatable

server_debug:
	$(GPRBUILD) -p -Pserver -XBUILD=debug -XKIND=static

server_release:
	$(GPRBUILD) -p -Pserver -XBUILD=release -XKIND=static

unit_tests_debug:
	$(GPRBUILD) -p -Punit_tests -XBUILD=debug -XKIND=static

unit_tests_release:
	$(GPRBUILD) -p -Punit_tests -XBUILD=release -XKIND=static

clean_libdb:
	$(GPRCLEAN) -f -q libdb.gpr -XBUILD=debug -XKIND=static || true
	$(GPRCLEAN) -f -q libdb.gpr -XBUILD=debug -XKIND=relocatable || true
	$(GPRCLEAN) -f -q libdb.gpr -XBUILD=release -XKIND=static || true
	$(GPRCLEAN) -f -q libdb.gpr -XBUILD=release -XKIND=relocatable || true

clean_server:
	$(GPRCLEAN) -f -q server.gpr -XBUILD=debug || true
	$(GPRCLEAN) -f -q server.gpr -XBUILD=release || true

clean_unit_tests:
	$(GPRCLEAN) -f -q unit_tests.gpr -XBUILD=debug || true
	$(GPRCLEAN) -f -q unit_tests.gpr -XBUILD=release || true

clean: clean_libdb clean_server clean_unit_tests

