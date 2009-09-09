# For each f in the list, Tests/f.adb must exist.
# A binary with its name is created in build/[release|debug]/.
TESTS=vectors

# Under Windows using Cygwin, I had problems with GNAT and GCC. As a result,
# I'm using the GNAT for Windows, not Cygwin. Because I didn't get the linking
# work otherwise, I'm using the GCC distributed with the GNAT for Windows 
# instead of the GCC available for Cygwin. (Note: for some reason, at some
# installations the linking between Cygwin's GCC and Windows' GNAT works.
# Nevertheless, I decided to stick with Windows' GNAT's GCC.)
ifeq ($(shell uname -o), Cygwin)
	CC=C:/Program\ Files/Gnat/Pentium/bin/gcc
	GNATMAKE=C:/Program\ Files/Gnat/Pentium/bin/gnatmake
else
	CC=/usr/gnat/bin/gcc
	GNATMAKE=gnatmake
endif


all: release debug

debug:
	for T in $(TESTS); do $(GNATMAKE) -Pcontainers -XBUILD=debug $$T.adb || exit; done

release:
	for T in $(TESTS); do $(GNATMAKE) -Pcontainers -XBUILD=release $$T.adb || exit; done

$(C_OBJECT_DIR)/%.o: $(C_DIR)/%.c
	$(CC) $(CFLAGS) $^ -o $@

apidoc: *.ads
	for F in *.ads; do adabrowse -f $$F; done

sstats:
	ls *.ads *.adb | xargs wc | sort -n

stats:
	ls *.ads *.adb | xargs wc | sort -n

statsbl:
	@grep -c ";" *.adb | sort -t \: -k 2 -n
	@echo -n "Sum: "
	@cat *.adb | grep -c ";"

clean:
	rm -f build/*/*

