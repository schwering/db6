TEST=$(shell gprbuild)
ifneq ($(TEST),) #Gnat 2009
	GPRBUILD=gprbuild -j2
	GPRCLEAN=gprclean
else # older Gnat
	GPRBUILD=gprmake
	GPRCLEAN=
endif

all: release debug

debug:
	$(GPRBUILD) -p -Pdb -XBUILD=debug

release:
	$(GPRBUILD) -p -Pdb -XBUILD=release

gcov_release:
	gcov -o libdb/release/objects libdb/release/objects/*.gcno
	#gcov -o bin/release bin/release/*.gcno

gcov_debug:
	gcov -o libdb/debug/objects libdb/debug/objects/*.gcno

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

statsbl:
	@grep -c ";" *.adb | sort -t \: -k 2 -n
	@echo -n "Sum: "
	@cat *.adb | grep -c ";"

clean:
	$(GPRCLEAN) -f -q db.gpr -XBUILD=debug || true
	$(GPRCLEAN) -f -q db.gpr -XBUILD=release || true
	$(GPRCLEAN) -f -q libdb.gpr -XBUILD=debug || true
	$(GPRCLEAN) -f -q libdb.gpr -XBUILD=release || true

