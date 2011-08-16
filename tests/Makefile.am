MAINTAINERCLEANFILES = Makefile.in
CLEANFILES = *~

check_SCRIPTS = test-allphot.bash
check_PROGRAMS =

TESTS = $(check_SCRIPTS)

dosed = @SED@ \
	-e 's,%BUILDDIR%,$(top_builddir),g' \
	-e 's,%CANONICALISE%,$(CANONICALISE),g' \
	-e 's,%SRCDIR%,$(top_srcdir),g'

EXTRA_DIST= \
	test-allphot.bash.in \
	generate.py \
	sky.dict \
	sky00.fits \
	sky00.list \
	sky01.fits \
	sky01.list \
	sky02.fits \
	sky02.list \
	sky03.fits \
	sky03.list \
	sky04.fits \
	sky04.list

SUFFIXES = .in
.in:
	@echo "Building $@..."
	@$(dosed) $< > $@
	@if test `expr match "$(check_SCRIPTS)" .*$@.*` != 0 ; then \
		chmod +x $@ ; \
	fi

clean-local:
	-rm -rf process_* *.psf *.als *s.fits *j.fits *.coo *.opt *.ap *.nei \
		*.lst *.pk *.raw *.mtr *.alf *j.fits *.tfr *.nmg *.mch *.cor \
		*.mag *.old allstar_*.in daophot_*.in daomatch_*.in \
		daomaster_*.in allframe_*.in \
		$(check_SCRIPTS)
