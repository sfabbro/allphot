allphotlibsdir = $(datadir)/@PACKAGE_NAME@/libs
allphotlibs_DATA = \
	core.bash \
	config.bash \
        output.bash \
	paths.bash \
	tests.bash \
	qa.bash \
	options.bash \
	daophot.bash \
	allframe.bash \
	default.allphot

EXTRA_DIST = \
	core.bash.in \
	config.bash.in \
        output.bash.in \
	paths.bash.in \
	tests.bash.in \
	options.bash.in \
	qa.bash.in \
	daophot.bash.in \
	allframe.bash.in \
	defaule.allphot.in

SUFFIXES = .in
dosed = @SED@ \
	-e 's,\@BASH\@,$(BASH),g' \
	-e 's,\@SED\@,@SED@,g' \
	-e 's,\@CANONICALISE\@,@CANONICALISE@,g' \
	-e 's,\@NAME\@,$(PACKAGE_NAME),g' \
	-e "s,\@DATADIR\@,$(datadir),g"

%.bash : %.bash.in
	@$(dosed) $< > $@

%.allphot : %.allphot.in
	cp $? $@

MAINTAINERCLEANFILES = Makefile.in
CLEANFILES = $(allphotlibs_DATA)
