#ifndef FITSKEY__H
#define FITSKEY__H

#define KEY_HDU_PRIMARY   2             /* only in primary header */
#define KEY_HDU_EXTENSION 4             /* only in extension header */


/* A fitskey struct is composed of:
  - name  : the name of the keyword in the header
  - value : its value as a string
  - comment: the associated comment
  - hdu   : to which HDU the key should belong to
  - datatype: kind of datatype to be
*/

typedef struct {
  char *name;
  char *value;
  int   datatype;
  int   hdu;
  char *comment;
} fitskey;


fitskey* fitskey_create();

fitskey* fitskey_alloc();

fitskey* fitskey_alloc_from_hdr(fitsfile *fptr, char *keyname, int *status);

fitskey* fitskey_read(char *keystring);

void fitskey_free(fitskey *key);

fitskey* fitskey_copy(fitskey *key);

fitskey* fitskey_replace(fitskey *dest, fitskey *src);

int fitskey_hdr_assign(fitsfile *fptr, fitskey *key, int *status);

int fitskey_update_hdr(fitsfile *fptr, fitskey *key, int *status);

void fitskey_print(FILE *stream, fitskey *key);

#endif /* FITSKEY__H */
