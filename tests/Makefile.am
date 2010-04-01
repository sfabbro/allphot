MAINTAINERCLEANFILES = Makefile.in
CLEANFILES = *~

check_SCRIPTS = test-allphot.bash
check_PROGRAMS =

TESTS = $(check_SCRIPTS)

EXTRA_DIST= \
	$(check_SCRIPTS) \
	generate.py \
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

clean-local:
	-rm -rf process_* *.psf *.als *s.fits *.coo *.opt *.ap *.nei *.lst *.pk
