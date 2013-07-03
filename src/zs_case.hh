#ifndef ZS_CASE_HH
#define ZS_CASE_HH

#include "config.h"

#if HAVE_STRCASECMP
# include <strings.h>
# define ZS_STRCASECMP strcasecmp
#else
int zs_strcasecmp(const char*, const char*);
# define ZS_STRCASECMP zs_strcasecmp
#endif

#endif // ZS_CASE_HH
