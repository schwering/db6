TEST=$(shell gprbuild)
ifneq ($(TEST),) #Gnat 2009
	GPRBUILD=gprbuild -gnat05 -j2
	GPRCLEAN=gprclean
else # older Gnat
	GPRBUILD=gprmake -gnat05
	GPRCLEAN=
endif

