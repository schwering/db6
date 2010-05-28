TEST=$(shell gprbuild)
ifneq ($(TEST),) #Gnat 2009
	GPRBUILD=gprbuild -j2
	GPRCLEAN=gprclean
else # older Gnat
	GPRBUILD=gprmake
	GPRCLEAN=
endif

all:
	$(GPRBUILD) -p -Pzlib

clean:
	$(GPRCLEAN) -f -q zlib.gpr

