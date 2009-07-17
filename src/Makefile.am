MAINTAINERCLEANFILES = Makefile.in
CLEANFILES = *~

EXTRA_DIST = arrays.inc

bin_PROGRAMS = daophot allstar daomaster daomatch montage2 allframe daogrow

daophot_SOURCES = \
	daophot.f pckpsf.f find.f fotometry.f \
	psf.f peak.f nstar.f fudge.f addstar.f substar.f \
	group.f sort.f lnxsubs.f fitsubs.f iosubs.f mathsubs.f
daophot_LDADD = @CFITSIO_LIBS@

allstar_SOURCES = allstar.f allstsubs.f lnxsubs.f \
	iosubs.f mathsubs.f fitsubs.f
allstar_LDADD = @CFITSIO_LIBS@

allframe_SOURCES = allframe.f fitsubs.f lnxsubs.f iosubs.f mathsubs.f
allframe_LDADD = @CFITSIO_LIBS@

montage2_SOURCES = montage2.f mathsubs.f iosubs.f lnxsubs.f fitsubs.f
montage2_LDADD = @CFITSIO_LIBS@

daomaster_SOURCES = daomaster.f iosubs.f mathsubs.f lnxsubs.f dummysm.f
daomatch_SOURCES = daomatch.f iosubs.f mathsubs.f lnxsubs.f
daogrow_SOURCES = daogrow.f iosubs.f mathsubs.f lnxsubs.f
