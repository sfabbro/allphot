MAINTAINERCLEANFILES = Makefile.in
CLEANFILES = *~

bin_SCRIPTS=allphot

libexecprogdir = $(libexecdir)/@PACKAGE@
libexecprog_SCRIPTS = \
	options.bash \
	recipes.bash \
	process.bash \
	daophot.bash \
	allframe.bash

EXTRA_DIST = $(bin_SCRIPTS) $(libexecprog_SCRIPTS)

SUFFIXES = .in
.in:
	@echo "  SED   " "$@"
	@@regex_cmd@ \
		-e "s,\@LIBEXECDIR\@,$(libexecdir),g" \
		-e "s,\@DATADIR\@,$(datadir),g" \
		"$<" > "$@"
