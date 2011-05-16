#ifndef STRINGUTILS__H
#define STRINGUTILS__H

#define UNDEFINED_DATE "INVALID"
#define UNDEFINED_MJD -99
#define UNDEFINED_RADEC -99
#define UNDEFINED_SECONDS -99
#define UNDEFINED_TYPE -99

/* time related routines */
void   date_to_isodate     (const char *datestr, char *isodatestr);
void   time_to_isotime     (const char *timestr, char *isotimestr);
void   date_time_to_isodate(const char *datestr, const char *timestr, char *isodatestr);
double gregorian_to_mjd     (int year, int month, int day, int hour, int min, double sec);
double gregorian_to_jd      (int year, int month, int day, int hour, int min, double sec);
void   jd_to_gregorian      (double jd, int *year, int *month, int *day, int *hour, int *min, double *sec);
void   mjd_to_gregorian     (double jd, int *year, int *month, int *day, int *hour, int *min, double *sec);

/* astrometry related routines */
double dec_string_to_degree(const char* decstr);
double ra_string_to_degree (const char* rastr);

/* pure string routines */
char* copystring    (const char *src);
char* replacestring (char *dest, const char *src);
char* substring     (const char *start, const char *end);
char* substringcln  (const char *start, const char *end);
int   stringnoc     (const char *src, int c);
int   stringsep2    (const char *src, char *first, char *second, int c);
int   removequotes  (char *dest, const char *src);
void  touppercase   (char *src);
void  tolowercase   (char *src);
char  first_non_blank_char(char *src);

#endif /* STRINGUTILS__H */
