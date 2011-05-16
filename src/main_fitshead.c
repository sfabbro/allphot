#include "fitsutils.h"
#include "executils.h"

static void usage(const char* progname, int status) {
  if (status != EXIT_SUCCESS) {
    fprintf(stderr, "Try `%s --help' for more information.\n", progname );
    exit(status);
  }

  printf("Usage: %s [OPTION]... FILE[ext]...\n", progname);
  printf("\nDump of the header(s) of the given FITS files. \n"
	 "If ext is given, dump only the header of that extension. \n"
	 "\nOPTIONS:\n");
  GETOPT_HV_USAGE;
  printf("\n");
  exit(status);
}


int main(int argc, char **argv)  {
  const char *prog_name = argv[0];

  if (argc <= 1) usage(prog_name, EXIT_FAILURE);

  struct option const long_opts[] = {
    GETOPT_HV_OPTIONS_STR,
    {NULL, 0, NULL, 0}
  };
  
  /* loop over arguments */
  int c;
  while ((c = getopt_long(argc, argv, "", long_opts, NULL)) != -1) {
    switch (c) {
      
    case 0:  /* long option */
      break;
      
      GETOPT_HV_OPTIONS_CASE(prog_name);

    default: 
      usage(prog_name,EXIT_FAILURE);
    }
  }

  int status, i, nrelhdu;
  fitsfile *fptr;

  /* loop over file names */
  while (optind<argc) { 

    fptr = fits_init(argv[optind++], READONLY, &nrelhdu, &status);
    if (!fptr) continue;

    /* print main HDU header */
    fits_print_header(fptr, &status);

    /* loop over other HDUs */
    for (i=0; i<nrelhdu; i++) {
      fits_movrel_hdu(fptr, 1, NULL, &status);
      fits_print_header(fptr, &status);
    }

    fits_close(fptr, &status);
  }
  
  return EXIT_SUCCESS;
}

