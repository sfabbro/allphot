bin_SCRIPTS = allphot parallel
EXTRA_DIST = allphot.in parallel.in
PACKAGE_UPNAME=ALLPHOT

dosed = @SED@ -e 's,\@BASH\@,$(BASH),g' \
	      -e 's,\@DATADIR\@,$(datadir),g' \
	      -e 's,\@VERSION\@,$(PACKAGE_VERSION),g' \
	      -e 's,\@NAME\@,$(PACKAGE_NAME),g' \
	      -e 's,\@UPNAME\@,$(PACKAGE_UPNAME),g' \

% : %.in
	@echo "Building $@..."
	@$(dosed) $< > $@
	@if test $@ = $(bin_SCRIPTS) ; then \
		chmod +x $@ ; \
	fi

MAINTAINERCLEANFILES = Makefile.in

install-data-local : $(foreach f, $(symlinks), install-symlink-$(f))

install-symlink-% : install-binSCRIPTS
	ln -snf $(bindir)/$(PACKAGE_NAME) \
		$(DESTDIR)$(bindir)/$*

CLEANFILES = $(bin_SCRIPTS)
