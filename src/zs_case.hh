#ifndef ZS_CASE_HH
#define ZS_CASE_HH

#include "config.h"

#define ZS_IDENTIFIER_CASE std::tolower

#if HAVE_STRCASECMP
# include <strings.h>
# define zs_strcasecmp strcasecmp
#else
int zs_strcasecmp(const char*, const char*);
#endif

int zs_charcasecmp(char, char);

#endif // ZS_CASE_HH
