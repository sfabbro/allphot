#include <string.h>
#include <math.h>
#include <ctype.h>
#include <fitsio.h>

#include "stringutils.h"
#include "fitsutils.h"
#include "fitskey.h"

fitsfile* fits_init(char *filename, int iomode, int *nrelhdu, int *status) {
  *status = 0;
  fitsfile *fptr;
  if (fits_open_file(&fptr, filename, iomode, status)) {
    fits_print_error(*status);
    return 0;
  }

  /* Get the current HDU position and number of HDUs*/
  int hdupos, nhdu;
  fits_get_hdu_num (fptr, &hdupos);
  fits_get_num_hdus(fptr, &nhdu, status);

  /* will process only a single header if a specific extension was given, 
     otherwise all HDUs */ 
  if  (hdupos != 1 || strchr(filename, '[') || nhdu == 1) 
    *nrelhdu = 0;
  else *nrelhdu = nhdu-hdupos;

  return fptr;
}


int fits_close(fitsfile *fptr, int *status) {

  int iomode;
  fits_file_mode(fptr, &iomode, status);
  if (iomode == READWRITE) {
    fits_write_date(fptr, status);
  }

  fits_close_file(fptr, status);
  fits_print_error(*status);

  return *status;
}

void fits_print_error(int status) {
  if (!status) return;
  static char status_str[FLEN_STATUS];
  static char errmsg[FLEN_ERRMSG];

  /* get the error description */
  fits_get_errstatus(status, status_str);
  fprintf(stderr, " Error: cfitsio %d: %s\n", status, status_str);

  /* get error stack messages */
  while (fits_read_errmsg(errmsg))
    fprintf(stderr, "   %s\n", errmsg);

}

int fits_print_header(fitsfile* fptr, int* status) {
  /* get # of keywords */
  int nkeys = 0;
  fits_get_hdrspace(fptr, &nkeys, 0, status);
  if (*status && nkeys == 0) return *status;

  /* Standard string lengths defined in fitsio.h */
  static char card[FLEN_CARD];
  
  int i;
  /* Read and print each keywords */
  for (i=1; i<=nkeys; i++) {
    if (fits_read_record(fptr, i, card, status)) break;
    printf("%s\n", card);
  }
  
  /* terminate listing with END */
  printf("END\n");
  
  fits_print_error(*status);

  return *status;
}

int fits_exist_key(fitsfile *fptr, char *keyname) {
  /* bulletproof */
  if (!fptr || !keyname) return 0;

  static char keyval[FLEN_VALUE];
  int status = 0;
  return fits_read_keyword(fptr, keyname, keyval, 0, &status) == KEY_NO_EXIST ? 0: 1;
}

int fits_print_keyvalue(fitsfile* fptr, char* keyname, int* status) {
  /* Standard string lengths defined in fitsio.h */
  static char keyval[FLEN_VALUE];
  
  int oldstatus = *status;

  /* Read and print requested keyword value */
  fits_read_keyword(fptr, keyname, keyval, 0, status);
  if (*status != KEY_NO_EXIST) {
    printf("%s", keyval);
  } else {
    *status = oldstatus;
    printf("absent");
  }

  return *status;
}

int fits_update_keyvalue(fitsfile* fptr, char* keystring, int* status) {
  static char keyname[FLEN_KEYWORD];
  static char keyval[FLEN_VALUE];

  /* decode the string, recognize the type, and update the key */
  if (!stringsep2(keystring, keyname, keyval, '=')) return *status;

  int datatype = fits_detect_key_type(keyval);
  fits_update_key_from_str(fptr, datatype, keyname, keyval, 0, status);
  fits_print_error(*status);

  return *status;
}

int fits_update_keyname(fitsfile* fptr, char* keystring, int* status) {
  static char oldname[FLEN_KEYWORD];
  static char newname[FLEN_KEYWORD];

  /* decode current name and new name, then modify it */
  if (!stringsep2(keystring, oldname, newname, '=')) return *status;
  fits_modify_name(fptr, oldname, newname, status);
  fits_print_error(*status);
  
  return *status;
}

int fits_update_keycomment(fitsfile* fptr, char* keystring, int* status) {
  static char keyname[FLEN_KEYWORD];
  static char keycomment[FLEN_COMMENT];

  /* decode current name and new name, then modify it */
  if (!stringsep2(keystring, keyname, keycomment, '=')) return *status;
  fits_modify_comment(fptr, keyname, keycomment, status);
  fits_print_error(*status);
  
  return *status;
}

int fits_update_keycard(fitsfile* fptr, char* keystring, int* status) {
  static char keyname[FLEN_KEYWORD];

  /* update directly the full card */
  int l;
  fits_get_keyname(keystring, keyname, &l, status);
  fits_update_card(fptr, keyname, keystring, status);
  fits_print_error(*status);
  
  return *status;
}

int fits_detect_key_type(char *keyval) {
  /* search for a ' */
  if (strchr(keyval, '\''))
    return TSTRING;
  else if (strchr(keyval,'.'))
    return TDOUBLE;
  else if (isdigit(keyval[0]))
    return TLONG;
  else if (strcasecmp(keyval, "TRUE")  == 0 || 
	   strcasecmp(keyval, "FALSE") == 0)
    return TLOGICAL;
  
  return TSTRING;
}

int fits_update_key_from_str(fitsfile* fptr, int datatype, char* name, char* valstr, char* comment, int* status) {
  if (*status >  0) return *status;

  if (datatype == UNDEFINED_TYPE) 
    datatype = fits_detect_key_type(valstr);

  float cf[2];
  double cd[2];
  char keyval[FLEN_VALUE];

  switch(datatype) {
  case TSTRING:
    removequotes(valstr, keyval);
    fits_update_key_str(fptr, name, keyval, comment, status);
    break;
  case TBYTE: case TSBYTE: case TUSHORT: case TSHORT: case TINT: case TLONG: 
    fits_update_key_lng(fptr, name, atol(valstr), comment, status);
    break;
  case TUINT: case TULONG:
    fits_update_key_lng(fptr, name, atof(valstr), comment, status);
    break;
  case TLOGICAL:
    fits_update_key_log(fptr, name, strcasecmp(valstr,"TRUE") == 0 ? TRUE: FALSE , comment, status);
    break;
  case TFLOAT:
    fits_update_key_flt(fptr, name, atof(valstr), -7, comment, status);
    break;
  case TDOUBLE:
    fits_update_key_dbl(fptr, name, atof(valstr), -15, comment, status);
    break;
  /* For the complex numbers, we assume the string format: 1+3i or 1+3j */
  case TCOMPLEX:
    if (sscanf(valstr,"(%f,%f)", cf, cf+1) == 2)
      fits_update_key_fixcmp(fptr, name, cf, -7, comment, status);
    else 
      fprintf(stderr, " Error in %s : wrong complex format in %s", __func__, valstr);
    break;
  case TDBLCOMPLEX:
    if (sscanf(valstr,"(%lf,%lf)", cd, cd+1) == 2)
      fits_update_key_dblcmp(fptr, name, cd, -15, comment, status);
    else 
      fprintf(stderr, " Error in %s : wrong complex format in %s", __func__, valstr);
    break;

  default: *status = BAD_DATATYPE;

  }

  return *status;
}
