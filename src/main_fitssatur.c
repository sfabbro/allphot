#include "fitsutils.h"
#include "executils.h"
#include <fitsio.h>

static void usage(const char* progname, int status) {
  if (status != EXIT_SUCCESS) {
    fprintf(stderr, "Try `%s --help' for more information.\n", progname );
    exit(status);
  }

  printf("Usage: %s [OPTION]... FILE[ext]...\n", progname);
  printf("Compute the saturation level of the given FITS files. \n"
	 "If ext is given, print only the satur level of that extension\n\n");
  printf(" -n,  --nofile                 do not print filename\n");
  GETOPT_HV_USAGE;
  printf("\n");
  exit(status);
}


int main(int argc, char **argv)  {

  float satur;
  int status, i, c, nrelhdu;
  int doprintfile = 1;
  fitsfile *fptr;
  char* filename;

  const char *prog_name = argv[0];

  if (argc <= 1) usage(prog_name, EXIT_FAILURE);

  struct option const long_opts[] = {
    {"nofile" , no_argument, 0, 'n'},
    GETOPT_HV_OPTIONS_STR,
    {NULL, 0, NULL, 0}
  };
  
  /* loop over arguments */
  while ((c = getopt_long(argc, argv, "n", long_opts, NULL)) != -1) {
    switch (c) {
      
    case 0:  /* long option */
      break;
      
    case 'n': 
      doprintfile = 0;
      break;

    GETOPT_HV_OPTIONS_CASE(prog_name);

    default: 
      usage(prog_name,EXIT_FAILURE);
    }
  }

  /* loop over file names */
  while (optind<argc) { 
    filename = argv[optind++];    
    fptr = fits_init(filename, READONLY, &nrelhdu, &status);
    if (!fptr) continue;

    /* loop over other HDUs */
    for (i=0; i<=nrelhdu; i++) {
      if (fits_get_satur(fptr, &satur, &status) > 0) continue;
      if (doprintfile) {
	printf(filename);
	if (nrelhdu > 0) printf("[%d]", fptr->HDUposition);
	printf(" ");
      }
      printf("%f\n", satur);
      if (i<nrelhdu) fits_movrel_hdu(fptr, 1, NULL, &status);
    }
    fits_close(fptr, &status);
  }
  
  return EXIT_SUCCESS;
}
