TEST=$(shell gprbuild)
ifneq ($(TEST),) #Gnat 2009
	GPRBUILD=gprbuild -j2
	GPRCLEAN=gprclean
else # older Gnat
	GPRBUILD=gprmake
	GPRCLEAN=
endif

all: tests_debug tests_release

libdb_debug:
	$(GPRBUILD) -p -Plibdb -XBUILD=debug -XKIND=static
	$(GPRBUILD) -p -Plibdb -XBUILD=debug -XKIND=relocatable

libdb_release:
	$(GPRBUILD) -p -Plibdb -XBUILD=release -XKIND=static
	$(GPRBUILD) -p -Plibdb -XBUILD=release -XKIND=relocatable

tests_debug:
	$(GPRBUILD) -p -Ptests -XBUILD=debug -XKIND=static

tests_release:
	$(GPRBUILD) -p -Ptests -XBUILD=release -XKIND=static

gcov_release:
	gcov -o libdb/release/static/objects libdb/release/static/objects/*.gcno
	#gcov -o bin/release bin/release/*.gcno

gcov_debug:
	gcov -o libdb/debug/static/objects libdb/debug/static/objects/*.gcno

gcov_clean:
	find . -name \*.gcda | xargs rm

tags: *.ads *.adb
	gnat xref -v -nostdinc -nostdlib -aOlibdb/debug/objects/ *.ad? | grep db- | grep \.ads | sort -fu >tags
	gnat xref -v -nostdinc -nostdlib -aOlibdb/debug/objects/ *.ads >tags

apidoc: *.ads
	for F in *.ads; do adabrowse -f $$F; done

sstats:
	ls *.ads *.adb | xargs wc | sort -n

stats:
	ls *.ads *.adb *.[hc] | xargs wc | sort -n

statsbt:
	find . | grep -E db-gen_btrees.\*\.ad\(s\|b\) | grep -v gen_btrees-gen | xargs wc | sort -n

statsbl:
	@grep -c ";" *.adb | sort -t \: -k 2 -n
	@echo -n "Sum: "
	@cat *.adb | grep -c ";"

clean:
	$(GPRCLEAN) -f -q tests.gpr -XBUILD=debug || true
	$(GPRCLEAN) -f -q tests.gpr -XBUILD=release || true
	$(GPRCLEAN) -f -q libdb.gpr -XBUILD=debug -XKIND=static || true
	$(GPRCLEAN) -f -q libdb.gpr -XBUILD=debug -XKIND=relocatable || true
	$(GPRCLEAN) -f -q libdb.gpr -XBUILD=release -XKIND=static || true
	$(GPRCLEAN) -f -q libdb.gpr -XBUILD=release -XKIND=relocatable || true

