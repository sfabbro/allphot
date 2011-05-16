#ifndef FITS_UTILS__H
#define FITS_UTILS__H

#include <fitsio.h>

/* initialize a fitsfile pointer from a file name. also get the number of relative HDUs to process */
fitsfile*  fits_init(char *filename, int iomode, int *nrelhdu, int *status);

/* close file and print left over errors */
int fits_close(fitsfile *fptr, int *status);

/* more standard error printer than the cfitsio standard one, directly to stderr */
void fits_print_error(int status);

/* dump the full header of a fitsfile on stdout */
int fits_print_header(fitsfile* fptr, int *status);

/* return 1 if a keyword exist */
int fits_exist_key(fitsfile*, char* keyname);

/* return 1 is current hdu is primary */
int fits_hdu_is_primary(fitsfile *fptr, int *status);

/* print the value of a keyword on stdout */
int fits_print_keyvalue(fitsfile* fptr, char *keyname, int* status);

/* update a full keyword card */
int fits_update_keycard(fitsfile* fptr, char* keystring, int* status);

/* update the key values. keystring is loaded as KEYNAME = NEWVALUE */
int fits_update_keyvalue(fitsfile* fptr, char* keystring, int* status);

/* update the key name. keystring is loaded as OLDKEYNAME = NEWKEYNAME */
int fits_update_keyname(fitsfile* fptr, char* keystring, int* status);

/* update the key comment. keystring is loaded as KEYNAME = NEWCOMMENT */
int fits_update_keycomment(fitsfile* fptr, char* keystring, int* status);

/* try to guess a cfistio type TDOUBLE, TSTRING,... from a string */
int fits_detect_key_type(char* valstring);

/* update the key from a string according to cfitsio standard */
int fits_update_key_from_str(fitsfile* fptr, int datatype, char* name, char* value, char* comment, int* status);

#endif /* FITS_UTILS__H */
