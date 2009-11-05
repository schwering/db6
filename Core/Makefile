# For each f in the list, Tests/f.adb must exist.
# A binary with its name is created in build/[release|debug]/.
HEAP_TESTS=heap cheap
TREE_TESTS=tree vtree dtree blob_ttree
MULTITHREADED_TREE_TESTS=ttree vttree cttree lruttree
CURSOR_TESTS=cursor controlled blob_cursor
THREAD_TESTS=thread thread2 thread3
COMPRESSION_TESTS=prefix levenshtein 
STRING_TESTS=unbounded_string
MIXED_TESTS=locks search hash modular rtest i_bm check data mkfs bitmap deflate

TESTS=$(HEAP_TESTS)\
      $(TREE_TESTS)\
      $(MULTITHREADED_TREE_TESTS)\
      $(BLOB_TREE_TESTS)\
      $(CURSOR_TESTS)\
      $(THREAD_TESTS)\
      $(COMPRESSION_TESTS)\
      $(STRING_TESTS)\
      $(MIXED_TESTS)


# Under Windows using Cygwin, I had problems with GNAT and GCC. As a result,
# I'm using the GNAT for Windows, not Cygwin. Because I didn't get the linking
# work otherwise, I'm using the GCC distributed with the GNAT for Windows 
# instead of the GCC available for Cygwin. (Note: for some reason, at some
# installations the linking between Cygwin's GCC and Windows' GNAT works.
# Nevertheless, I decided to stick with Windows' GNAT's GCC.)
#ifeq ($(shell uname -o), Cygwin)
	#CC=C:/Program\ Files/Gnat/Pentium/bin/gcc
	#GNATMAKE=C:/Program\ Files/Gnat/Pentium/bin/gnatmake
#else
	GPRBUILD=gprbuild
#endif

all: release debug

debug:
	for T in $(TESTS); do $(GPRBUILD) -p db.gpr -XBUILD=debug $$T.adb || exit; done

release:
	for T in $(TESTS); do $(GPRBUILD) -p db.gpr -XBUILD=release $$T.adb || exit; done

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

