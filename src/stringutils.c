#include <ctype.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdio.h>

#include "stringutils.h"

void date_to_isodate(const char* datestr, char* isodatestr) {
  int year, month, day;
  if ((sscanf(datestr, "%d/%d/%d", &year, &month, &day) != 3) &&
      (sscanf(datestr, "%d-%d-%d", &year, &month, &day) != 3)) {
    strcpy(UNDEFINED_DATE, isodatestr);
    return;
  }

  if (day > 31) { int tmp = year; year = day; day = tmp; }
  else if (year > 31) ;
  else {
    strcpy(UNDEFINED_DATE, isodatestr);
    return;
  }

  /* convert 2 digits dates */
  if (year < 1949) year += (year>50) ? 1900 : 2000;
  sprintf(isodatestr, "%04d-%02d-%02d", year, month, day);
}

/* HH:MM:SS[.sss]*/
void time_to_isotime(const char* timestr, char* isotimestr) {  
  int hour, min;
  double sec;
  if (sscanf(timestr, "%d:%d:%lf", &hour, &min, &sec) != 3) {
    strcpy(UNDEFINED_DATE, isotimestr);
    return;
  }
  sprintf(isotimestr,"%02d:%02d:%06.3lf", hour, min, sec);
}

void date_time_to_isodate(const char* datestr, const char* timestr, char* isodatestr) {
  date_to_isodate(datestr, isodatestr);
  if (strcmp(isodatestr, UNDEFINED_DATE) == 0) return;
  char* stdtimestr = strchr(isodatestr, '\0');
  *stdtimestr++ = 'T';
  time_to_isotime(timestr, stdtimestr);
}

double dec_string_to_degree(const char* decstr) {

  /* first case, assume DEC in DEC:MIN:SEC, separators may be either : or ' */
  char dec[64];
  char *p;
  strcpy(dec, decstr);
  double minus_char = 1; /* no minus sign */
  for (p = dec; *p; p++) {
    if (*p == ':' || *p == '\'' ) *p = ' ';
    if (*p == '-') minus_char = -1.;
  }

  int deg, minutes; 
  double seconds;

  if (sscanf(dec,"%d %d %lf",&deg, &minutes, &seconds) == 3) {
      double sign = 1;
      if (deg < 0) sign = -1.;
      else if (deg == 0) sign = minus_char;
      return sign * (fabs((double)(deg)) +
		     fabs((double)(minutes))/60. +
		     fabs(seconds)/3600.);
  }

  /* second case, assume the dec string to contain the value in degrees already */
  char *endptr;
  double decdeg = strtod(decstr, &endptr);
  if (decstr != endptr) /* successful conversion by strtod */
    return decdeg;

  fprintf(stderr, " %s : Error cannot decode a declination in: %s ", __func__, decstr);
  return UNDEFINED_RADEC;
}

double ra_string_to_degree(const char* rastr) {
  /* first case, assume RA in HR:MIN:SEC, separators may be either : or ' */
  int hours, minutes; 
  double seconds;
  if (sscanf(rastr, "%d:%d:%lf", &hours, &minutes, &seconds) == 3) {
    return 15. * ((double)(hours) +
		  (double)(minutes)/60. +
		  seconds/3600.);
  }


  /* second case, assume the ra string to contain the value in degrees already */
  char *endptr;
  double ra = strtod(rastr, &endptr);
  if (endptr != rastr)  /* successful conversion by strtod */
    return ra;

  fprintf(stderr, " %s : Error cannot decode a right ascension in: %s ", __func__, rastr);
  return UNDEFINED_RADEC;
}

/* loosely inspired from jday.sf.net */
double gregorian_to_jd(int year, int month, int day, int hour, int min, double sec) {  
  /* decimal day fraction */
  double frac = ((double) hour/ 24.0)
              + ((double) min / 1440.0)
              + (         sec / 86400.0);

  /* convert date to format YYYY.MMDDdd	*/
  double gyr =    (double) year
      + (0.01   * (double) month)
      + (0.0001 * (double) day)
      + (0.0001 * frac) + 1.0e-9;

  /* conversion factors */
  long iy0, im0;
  if (month <= 2) {
    iy0 = year - 1L;
    im0 = month + 12;
  }
  else {
    iy0 = year;
    im0 = month;
  }

  long ia = iy0 / 100L;
  long ib = 2L - ia + (ia >> 2);

  /* calculate julian date */
  long jd;
  if (year <= 0)
    jd = (long) ((365.25 * (double) iy0) - 0.75)
       + (long) (30.6001 * (im0 + 1L) )
       + (long) day + 1720994L;
  else
    jd = (long) (365.25 * (double) iy0)
       + (long) (30.6001 * (double) (im0 + 1L))
       + (long) day + 1720994L;

  if  (gyr >= 1582.1015)	/* on or after 15 October 1582	*/
    jd += ib;

  return (double) jd + frac + 0.5;
}

double gregorian_to_mjd(int year, int month, int day, int hour, int min, double sec) {
  return gregorian_to_jd(year, month, day, hour, min, sec) - 2400000.5;
}


void jd_to_gregorian(double jd, int* year, int* month, int* day, int* hour, int* min, double* sec) {
  /* integer julian date */
  long ijd = (long) (jd + 0.5);

  /* decimal day fraction */
  double frac = jd + 0.5 - (double) ijd + 1.0e-10;

  /* conversion factors */
  long ka = ijd;
  long ialp;
  if (ijd >= 2299161L ) {
    ialp = ( (double) ijd - 1867216.25 ) / ( 36524.25 );
    ka = ijd + 1L + ialp - ( ialp >> 2 );
  }
  long kb = ka + 1524L;
  long kc =  ( (double) kb - 122.1 ) / 365.25;
  long kd = (double) kc * 365.25;
  long ke = (double) ( kb - kd ) / 30.6001;

  *day = kb - kd - ((long) ( (double) ke * 30.6001 ));
  if ( ke > 13L )
    *month = ke - 13L;
  else
    *month = ke - 1L;
  if ( (*month == 2) && (*day > 28) )
    *day = 29;
  if ( (*month == 2) && (*day == 29) && (ke == 3L) )
    *year = kc - 4716L;
  else if ( *month > 2 )
    *year = kc - 4716L;
  else
    *year = kc - 4715L;

  double dh = frac * 24.0;
  *hour = dh;
  double dm = (dh - (double) *hour ) * 60.0;
  *min  = dm;
  *sec = (dm - (double) *min) * 60.0;
}

void mjd_to_gregorian(double jd, int *year, int* month, int* day, int* hour, int* min, double* sec) {
  jd_to_gregorian(jd + 2400000.5, year, month, day, hour, min, sec);
}

char* copystring(const char* src) {
  /* bulletproof */
  if (!src) return 0;

  size_t len = strlen(src) + 1;
  char* dest = 0;

  if (len > 0)
    dest = malloc(len);

  if (!dest) return 0;

  return strcpy(dest, src);
}

char* replacestring(char *dest, const char* src) {
  if (!src)  return 0;  
  if (dest) free(dest);
  dest = copystring(src);
  return dest;
}


char* substring(const char *start, const char *end) {
  char* result = malloc(end - start + 1);
  char* scan_result = result;
  const char *scan = start;

  while (scan < end)
    *scan_result++ = *scan++;

  *scan_result = 0;
  return result;
}

char* substringcln(const char *start, const char *end) {
  char* result = malloc(end - start + 1);
  char* scan_result = result;
  const char *scan = start;
  while (scan < end) {
    if (isgraph(*scan)) *scan_result++ = *scan;
    scan++;
  }
  *scan_result = 0;
  return result;
}

int stringnoc(const char* src, int c) {
  const char *ps = src;
  int count = 0;
  while (*ps != '\0') if (*ps++ == c) count++;
  return count;
}


int stringsep2(const char* src, char* first, char* second, int c) {  
  char* sep = strchr(src, c);

  if (!sep) {
    fprintf(stderr, " Error: unable to separate %s with '%c' \n", src, c);
    return 0;
  }

  /* everything after the separator is the second */
  strcpy(second, sep+1);

  /* copy the first until the separator */
  char *pfirst = first;
  const char *pcopy  = src;
  while (pcopy<sep) *pfirst++ = *pcopy++;
  *pfirst = '\0';

  /* strip white spaces */
  sscanf(first, "%s", first);
  sscanf(second, "%s", second);


  return 1;
}

int removequotes(char *dest, const char *src) {
  int status = 0;

  ffc2s(src, dest, &status);

  return status;
}

void touppercase(char *src) {
  if (!src) return;

  while (*src != '\0') {
    *src = toupper(*src);
    src++;
  }
}

void tolowercase(char *src) {
  if (!src) return;

  while (*src != '\0') {
    *src = tolower(*src);  
    src++;
  }
}


char first_non_blank_char(char *src) {
  if (!src) return 0;
  while (isblank(*src)) src++;
  return *src;
}
