LIBDB_DIR         =libdb
TEST_DIR          =bin

RELEASE_DIR       =release
DEBUG_DIR         =debug
OBJECT_DIR        =objects

DATA_DIR          =.tmp

DIRS=$(LIBDB_DIR)\
     $(LIBDB_DIR)/$(DEBUG_DIR)\
     $(LIBDB_DIR)/$(DEBUG_DIR)/$(OBJECT_DIR)\
     $(LIBDB_DIR)/$(RELEASE_DIR)\
     $(LIBDB_DIR)/$(RELEASE_DIR)/$(OBJECT_DIR)\
     $(TEST_DIR)\
     $(TEST_DIR)/$(RELEASE_DIR)\
     $(TEST_DIR)/$(DEBUG_DIR)\
     $(DATA_DIR)

C_OBJECTS=db-compression-deflate-c db-io-low_level-c db-util-timers-c


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

debug: dirs cobjs
	for T in $(TESTS); do $(GNATMAKE) -Pdb -XBUILD=debug $$T.adb || exit; done

release: dirs cobjs
	for T in $(TESTS); do $(GNATMAKE) -Pdb -XBUILD=release $$T.adb || exit; done

libdb_release: dirs
	for F in $(C_OBJECTS); do make $(LIBDB_DIR)/$(RELEASE_DIR)/$(OBJECT_DIR)/$$F.o || exit; done
	$(GNATMAKE) -Plibdb -XBUILD=release

libdb_debug: dirs
	for F in $(C_OBJECTS); do make $(LIBDB_DIR)/$(DEBUG_DIR)/$(OBJECT_DIR)/$$F.o || exit; done
	$(GNATMAKE) -Plibdb -XBUILD=debug

dirs:
	for D in $(DIRS); do if test \! -d $$D; then mkdir $$D; fi done

$(LIBDB_DIR)/$(RELEASE_DIR)/$(OBJECT_DIR)/%.o: %.c
	$(CC) $(CFLAGS) $^ -o $@

$(LIBDB_DIR)/$(DEBUG_DIR)/$(OBJECT_DIR)/%.o: %.c
	$(CC) $(CFLAGS) $^ -o $@

tags: *.ads *.adb Makefile
	gnat xref -v -nostdinc -nostdlib *.ad[s] | grep db- | grep \.ads >tags
#	| sort -fu >tags

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
	rm -rf $(LIBDB_DIR) $(TEST_DIR)

