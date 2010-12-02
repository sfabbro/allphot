bin_SCRIPTS = allphot
EXTRA_DIST = allphot.in

dosed = @SED@ \
	-e 's,%BASH%,$(BASH),g' \
	-e 's,%DATADIR%,$(datadir),g' \
	-e 's,%LIBEXECDIR%,$(libexecdir),g' \
	-e 's,%VERSION%,$(PACKAGE_VERSION),g' \
	-e 's,%NAME%,$(PACKAGE_NAME),g'

SUFFIXES = .in
.in:
	@echo "Building $@..."
	@$(dosed) $< > $@
	@if test `expr match "$(bin_SCRIPTS)" .*$@.*` != 0 ; then \
		chmod +x $@ ; \
	fi

MAINTAINERCLEANFILES = Makefile.in
CLEANFILES = $(bin_SCRIPTS)
