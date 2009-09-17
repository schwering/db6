C_DIR=C
C_OBJECT_DIR=$(C_DIR)/objects
C_OBJECTS=db-io-low_level db-util-timers


# For each f in the list, Tests/f.adb must exist.
# A binary with its name is created in build/[release|debug]/.
HEAP_TESTS=heap cheap
TREE_TESTS=tree vtree dtree blob_ttree
MULTITHREADED_TREE_TESTS=ttree vttree cttree
CURSOR_TESTS=cursor controlled blob_cursor
THREAD_TESTS=thread thread2 thread3
COMPRESSION_TESTS=prefix levenshtein 
STRING_TESTS=unbounded_string
MIXED_TESTS=locks search hash modular rtest i_bm check data mkfs bitmap

TESTS=$(HEAP_TESTS)\
      $(TREE_TESTS)\
      $(MULTITHREADED_TREE_TESTS)\
      $(BLOB_TREE_TESTS)\
      $(CURSOR_TESTS)\
      $(THREAD_TESTS)\
      $(COMPRESSION_TESTS)\
      $(STRING_TESTS)\
      $(MIXED_TESTS)

# Set the DEFINITIONS variable appropriate to the operating system.
# They're used for steering the C low level routines.
ifeq ($(shell uname), Darwin)
	DEFINITIONS=
else
	DEFINITIONS=-DHAVE_OFF64_T
endif


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


COMMONFLAGS=-Wall\
	    -D_FILE_OFFSET_BITS=64\
	    -D_GNU_SOURCE\
	    $(DEFINITIONS)\
	    #-DDB_DIRECT_IO
CFLAGS=-c $(COMMONFLAGS)

all: release debug

debug: cobjs
	for T in $(TESTS); do $(GNATMAKE) -Pdb -XBUILD=debug $$T.adb || exit; done

release: cobjs
	for T in $(TESTS); do $(GNATMAKE) -Pdb -XBUILD=release $$T.adb || exit; done

cobjs:
	for F in $(C_OBJECTS); do make $(C_OBJECT_DIR)/$$F.o || exit; done

$(C_OBJECT_DIR)/%.o: $(C_DIR)/%.c
	$(CC) $(CFLAGS) $^ -o $@

tags: *.ads *.adb Makefile
	gnat xref -v -nostdinc -nostdlib *.ad[s] | grep db- | grep \.ads >tags
#	| sort -fu >tags

apidoc: *.ads
	for F in *.ads; do adabrowse -f $$F; done

sstats:
	ls *.ads *.adb | xargs wc | sort -n

stats:
	ls *.ads *.adb C/*.[hc] | xargs wc | sort -n

statsbl:
	@grep -c ";" *.adb | sort -t \: -k 2 -n
	@echo -n "Sum: "
	@cat *.adb | grep -c ";"

clean:
	rm -f build/*/*
	rm -f $(C_OBJECT_DIR)/*.o
