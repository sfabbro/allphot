#include <string.h>
#include <fitsio.h>

#include "stringutils.h"
#include "fitsutils.h"
#include "fitskey.h"

fitskey* fitskey_create() {
  fitskey *key = malloc(sizeof(fitskey));

  key->name     = 0;
  key->value    = 0;
  key->comment  = 0;
  key->datatype = UNDEFINED_TYPE;
  key->hdu      = KEY_HDU_PRIMARY | KEY_HDU_EXTENSION;

  return key;
}

fitskey* fitskey_alloc() {
  fitskey *key = fitskey_create();

  key->name    = malloc(FLEN_KEYWORD+1);
  key->value   = malloc(FLEN_VALUE+1);
  key->comment = malloc(FLEN_COMMENT+1);

  return key;
}

fitskey* fitskey_alloc_from_hdr(fitsfile *fptr, char *keyname, int *status) {
  /* bulletproof */
  if (!fptr || !keyname || *status > 0) return 0;

  /* no such keyname in header */
  if (fits_exist_key(fptr, keyname) == 0) return 0;

  fitskey *key = fitskey_create();
  
  key->name    = copystring(keyname);
  key->value   = malloc(FLEN_VALUE+1);
  key->comment = malloc(FLEN_COMMENT+1);

  fits_read_keyword(fptr, keyname, key->value, key->comment, status);

  key->datatype = fits_detect_key_type(key->value);

  return key;
}

fitskey* fitskey_read(char *keystring) {
  /* bulletproof */
  if (!keystring) return 0;

  fitskey *key = fitskey_alloc();
  
  /* extremely basic column parsing */
  if (sscanf(keystring, "%s %s %d %d %s", key->name, key->value, &key->datatype, &key->hdu, key->comment) != 5)
    return 0;

  return key;
}

void fitskey_free(fitskey *key) {
  /* bulletproof */
  if (!key) return;

  if (key->name)    free(key->name);
  if (key->value)   free(key->value);
  if (key->comment) free(key->comment);

  free(key);
}

fitskey* fitskey_copy(fitskey *key) {
  if (!key) return 0;

  fitskey *cpy  = fitskey_create();

  cpy->name     = copystring(key->name);
  cpy->value    = copystring(key->value);
  cpy->datatype = key->datatype;
  cpy->hdu      = key->hdu;
  cpy->comment  = copystring(key->comment);

  return cpy;
}


fitskey* fitskey_replace(fitskey *dest, fitskey *src) {
  /* bulletproof */
  if (!dest || !src) return 0;

  /* copy name only if present in src */
  if (src->name && !dest->name)
    dest->name = copystring(src->name);
  
  /* copy value only if present in src */
  if (src->value) {
    if (dest->value) free(dest->value);
    dest->value = copystring(src->value);
  }

  /* copy comment only if absent in dest */
  if (src->comment && !dest->comment)
    dest->comment = copystring(src->comment);
  
  /* copy datatype only of undefined */
  if (dest->datatype == UNDEFINED_TYPE)
    dest->datatype = src->datatype;
  
  /* copy hdu no matter what */
  dest->hdu = src->hdu;

  return dest;
}

int fitskey_hdr_assign(fitsfile* fptr, fitskey* key, int* status) {
  /* bulletproof */
  if (!fptr || !key || *status > 0) return *status;

  if (strlen(key->name) == 0) {
    fprintf(stderr, " Error in %s : undefined key name to assign\n", __func__);
    return *status;
  }

  if (fits_exist_key(fptr, key->name) == 0) return *status;

  /* do not assign if not in proper hdu */
  int chdu;
  fits_get_hdu_num(fptr, &chdu);
  if ((chdu == 1  && !(key->hdu | KEY_HDU_PRIMARY))
   || (chdu >  1  && !(key->hdu | KEY_HDU_EXTENSION)))
    return *status;

  /* read card value */
  if (key->value) free(key->value);
  key->value = malloc(FLEN_VALUE+1);

  fits_read_keyword(fptr, key->name, key->value, 0, status);

  /* read card comment if undefined */
  if (!key->comment) {
    key->comment = malloc(FLEN_COMMENT+1);
    fits_read_keyword(fptr, key->name, key->value, key->comment, status);
  }
  
  /* detect datatype if undefined */
  if (key->datatype == UNDEFINED_TYPE)
    key->datatype = fits_detect_key_type(key->value);

  return *status;
}

int fitskey_update_hdr(fitsfile* fptr, fitskey* key, int* status) {
  /* bulletproof */
  if (!fptr || !key || *status >0) return *status;

  fits_update_key_from_str(fptr, key->datatype, key->name, key->value, key->comment, status);

  return *status;
}

void fitskey_print(FILE *stream, fitskey *key) {
  if (!key) return;

  if (key->name)
    fprintf(stream, "%-8s= ", key->name);
    
  if (key->value)
    fprintf(stream, key->value);
  else
    fprintf(stream, "ABSENT        ");

  if (key->comment)
    fprintf(stream, " / %s", key->comment);  
}
