#include "executils.h"
#include "fitsutils.h"

static void usage(const char* progname, int status) {
  if (status != EXIT_SUCCESS) {
    fprintf(stderr, "Try `%s --help' for more information.\n", progname);
    exit(status);
  }


  printf("Usage: %s [OPTIONS]... FILE[ext]...\n", progname);
  printf("\nPrint, set, change or delete keys of the given FITS files.\n"
	 "If ext is given, process only the keywords of that extension.\n"
         "\nOPTIONS:\n");  
  printf(" -p,  --print KEY              print key \n"
	 " -n,  --nofile                 do not print filename\n"
	 " -f,  --full  KEY=\"KEY=...\"    update the full key entry \n"
	 " -v,  --value KEY=NEWVALUE     update key value (use it to modify type too)\n"
	 " -k,  --keyname KEY=NEWNAME    update key name \n"
	 " -c,  --comment KEY=COMMENT    update key comment \n"
	 " -d,  --delete KEY             delete the key \n");
  GETOPT_HV_USAGE;
  printf("\n");
  exit(status);
}

typedef struct { char* keystring; int dofunc; } keyprocess;

int main(int argc, char** argv) {
  const char* prog_name = argv[0];
  if (argc <= 1) usage(prog_name, EXIT_FAILURE);

  static struct option const long_opts[] = {
    {"print" , required_argument, 0, 'p'},
    {"nofile" , no_argument, 0, 'n'},
    {"full"  , required_argument, 0, 'f'},
    {"value" , required_argument, 0, 'v'},
    {"delete", required_argument, 0, 'd'},
    {"keyname"  , required_argument, 0, 'k'},
    {"comment", required_argument, 0, 'c'},
    GETOPT_HV_OPTIONS_STR,
    {0, 0, 0, 0}
  };
  
  int c;
  int doprintfile = 1;
  int doprint = 2, dofull = 4, doval = 8, dodel = 16, doname = 32, docom = 64;
  int nkeys = 0;
  int iomode = READONLY;

  keyprocess* keys = malloc((argc-1)*sizeof(keyprocess));
  keyprocess* keyscan = keys;

  /* loop over arguments */
  while ((c = getopt_long(argc, argv, "p:nf:v:d:k:c:", long_opts, 0)) != -1) {
    switch (c) {
      
    case 0: /* long option */
      break;
      
    case 'p': 
      if (optarg) {
	keyscan->keystring = optarg;
	keyscan->dofunc  |= doprint;
	keyscan++; nkeys++;
      }
      break;
      
    case 'n': 
      doprintfile = 0;
      break;

    case 'f': 
      if (optarg) {
	keyscan->keystring = optarg;
	keyscan->dofunc  |= dofull;
	keyscan++; nkeys++;
	iomode = READWRITE;
      }
      break;

    case 'v': 
      if (optarg) {
	keyscan->keystring = optarg;
	keyscan->dofunc  |= doval;
	keyscan++; nkeys++;
	iomode = READWRITE;
      }
      break;
      
    case 'd': 
      if (optarg) {
	keyscan->keystring = optarg;
	keyscan->dofunc  |= dodel;
	keyscan++; nkeys++;
	iomode = READWRITE;
      }
      break;
      
    case 'k': 
      if (optarg) {
	keyscan->keystring = optarg;
	keyscan->dofunc  |= doname;
	keyscan++; nkeys++;
	iomode = READWRITE;
      }
      break;
      
    case 'c': 
      if (optarg) {
	keyscan->keystring = optarg;
	keyscan->dofunc  |= docom;
	keyscan++; nkeys++;
	iomode = READWRITE;
      }
      break;

    GETOPT_HV_OPTIONS_CASE(prog_name);
    
    default: 
      usage(prog_name,EXIT_FAILURE);      
    }
  }

  fitsfile* fptr;
  char* filename;
  int i, j, status, nrelhdu;

  while (optind<argc) {              /* loop over file names */

    filename = argv[optind++];
    fptr = fits_init(filename, iomode, &nrelhdu, &status);
    if (!fptr) continue;

    for (i=0; i<=nrelhdu; i++) {     /* loop over HDU of each file */

      /* printing case */
      if (iomode == READONLY && doprintfile) { 
	printf(filename);
	if (nrelhdu > 0) printf("[%d]", fptr->HDUposition);
	printf(" ");
      }

      for (j=0; j<nkeys; j++) {         /* loop over keys of each HDU */
	
	keyscan = &keys[j];

	if (keyscan->dofunc & doprint) {
	  fits_print_keyvalue(fptr, keyscan->keystring, &status);
	  printf(" ");
	}
	if (keyscan->dofunc & dofull)  
	  fits_update_keycard(fptr, keyscan->keystring, &status);
	
	if (keyscan->dofunc & doval)  
	  fits_update_keyvalue(fptr, keyscan->keystring, &status);
	
	if (keyscan->dofunc & dodel)   
	  fits_delete_key(fptr, keyscan->keystring, &status);
	
	if (keyscan->dofunc & doname)
	  fits_update_keyname(fptr, keyscan->keystring, &status);
	
	if (keyscan->dofunc & docom)    
	  fits_update_keycomment(fptr, keyscan->keystring, &status);
	
      } /* end loop on keys */

      /* printing case */
      if (iomode == READONLY) printf("\n");

      if (i<nrelhdu) fits_movrel_hdu(fptr, 1, 0, &status);

    } /* end loop on HDUs */

    fits_close(fptr, &status);
  } /* end loop on files */

  free(keys);

  return EXIT_SUCCESS;
}
