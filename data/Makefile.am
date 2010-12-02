optionsdir = $(datadir)/@PACKAGE_NAME@/options
options_DATA = \
	daophot.opt \
	photo.opt \
	allstar.opt \
	daomaster.opt \
	allframe.opt

sexdir = $(datadir)/@PACKAGE_NAME@/sextractor
sex_DATA = \
	default.sex \
	default.param \
	default.nnw \
	default.conv


dictdir = $(datadir)/@PACKAGE_NAME@/dict
dict_DATA = \
	macho.dict

EXTRA_DIST = $(options_DATA) $(sex_DATA) $(dict_DATA)

MAINTAINERCLEANFILES = Makefile.in
CLEANFILES = *~
