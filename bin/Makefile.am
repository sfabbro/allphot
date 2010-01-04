bin_SCRIPTS = allphot parallel
EXTRA_DIST = allphot.in parallel.in

dosed = @SED@ -e 's,\@BASH\@,$(BASH),g' \
	      -e 's,\@DATADIR\@,$(datadir),g' \
	      -e 's,\@VERSION\@,$(VERSION),g'

% : %.in
	@echo "Building $@..."
	@$(dosed) $< > $@
	@if test $@ = $(bin_SCRIPTS) ; then \
		chmod +x $@ ; \
	fi

MAINTAINERCLEANFILES = Makefile.in

install-data-local : $(foreach f, $(symlinks), install-symlink-$(f))

install-symlink-% : install-binSCRIPTS
	ln -snf $(bindir)/eselect \
		$(DESTDIR)$(bindir)/$*

CLEANFILES = $(bin_SCRIPTS)
