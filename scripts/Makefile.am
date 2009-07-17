MAINTAINERCLEANFILES = Makefile.in
CLEANFILES = *~

#bin_SCRIPTS=allphot
libexecprogdir = $(libexec)/@PACKAGE@
libexecprog_SCRIPTS = \
	options.bash \
	recipes.bash \
	process.bash \
	daophot.bash \
	allframe.bash

allphotdir = $(datadir)
allphot_DATA= \
	allstar.opt \
	allframe.opt \
	photo.opt \
	daophot.opt

EXTRA_DIST = $(bin_SCRIPTS) $(libexecprog_SCRIPTS) $(noinst_DATA)

check_SCRIPTS = test-allphot.bash

check_PROGRAMS =

TESTS = $(check_SCRIPTS)

clean-local:
	-rm -rf tests
