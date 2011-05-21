allphotlibsdir = $(libexecdir)/@PACKAGE_NAME@/libs

allphotlibs_SCRIPTS = \
	default.allphot \
	core.bash \
	config.bash \
        output.bash \
	paths.bash \
	tests.bash \
	qa.bash \
	options.bash \
	daophot.bash \
	allframe.bash \
	process.bash

EXTRA_DIST = \
	default.allphot.in \
	core.bash.in \
	config.bash.in \
        output.bash.in \
	paths.bash.in \
	tests.bash.in \
	options.bash.in \
	qa.bash.in \
	process.bash.in \
	daophot.bash.in \
	allframe.bash.in \
	process.bash.in

dosed = @SED@ \
	-e 's,%BASH%,$(BASH),g' \
	-e 's,%SED%,@SED@,g' \
	-e 's,%CANONICALISE%,@CANONICALISE@,g' \
	-e 's,%VERSION%,$(PACKAGE_VERSION),g' \
	-e 's,%NAME%,$(PACKAGE_NAME),g' \
	-e "s,%DATADIR%,$(datadir),g"

SUFFIXES = .in
.in:
	@echo "Building $@..."
	@$(dosed) $< > $@

MAINTAINERCLEANFILES = Makefile.in
CLEANFILES = $(allphotlibs_SCRIPTS)
