GPRBUILD=gprbuild
GPRCLEAN=gprclean

all: release debug

debug:
	$(GPRBUILD) -p db.gpr -XBUILD=debug

release:
	$(GPRBUILD) -p db.gpr -XBUILD=release

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

