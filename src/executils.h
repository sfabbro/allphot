#ifndef EXECUTILS__H
#define EXECUTILS__H

#include <getopt.h>
#include <limits.h>

#ifdef HAVE_CONFIG_H
#include	"config.h"
#endif

#define GETOPT_HV_USAGE \
 printf("      --help         display this help and exit\n" \
	"      --version      output version information and exit\n")

enum
{
  GETOPT_HELP_CHAR    = (CHAR_MIN - 2),
  GETOPT_VERSION_CHAR = (CHAR_MIN - 3)
};

#define GETOPT_HV_OPTIONS_STR \
  {"help",    no_argument, NULL, GETOPT_HELP_CHAR},\
  {"version", no_argument, NULL, GETOPT_VERSION_CHAR}


#define GETOPT_HV_OPTIONS_CASE(prog_name) \
    case GETOPT_HELP_CHAR: \
      usage( prog_name, EXIT_SUCCESS); \
      break; \
    case GETOPT_VERSION_CHAR: \
      fprintf(stdout, "%s version %s \n", prog_name, VERSION); \
      exit(EXIT_SUCCESS);\
      break;


#endif /* EXECUTILS__H */
