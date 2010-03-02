bin_SCRIPTS = allphot parallel 
EXTRA_DIST = allphot.in parallel.in

dosed = @SED@ -e 's,\@BASH\@,$(BASH),g' \
	      -e 's,\@DATADIR\@,$(datadir),g' \
	      -e 's,\@VERSION\@,$(PACKAGE_VERSION),g' \
	      -e 's,\@NAME\@,$(PACKAGE_NAME),g'

% : %.in
	@echo "Building $@..."
	@$(dosed) $< > $@
	@if test $@ = $(bin_SCRIPTS) ; then \
		chmod +x $@ ; \
	fi

MAINTAINERCLEANFILES = Makefile.in

CLEANFILES = $(bin_SCRIPTS)
