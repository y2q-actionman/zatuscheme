#ifndef ZS_CASE_HH
#define ZS_CASE_HH

#include <cctype>
#include "config.h"

#ifdef USE_CASE_UPPER
# define ZS_CASE(c) std::toupper(c)
#elif USE_CASE_LOWER
# define ZS_CASE(c) std::tolower(c)
#else
# define ZS_CASE(c) std::tolower(c)
#endif

#if HAVE_STRCASECMP
# include <strings.h>
# define ZS_STRCASECMP strcasecmp
#else
int zs_strcasecmp(const char*, const char*);
# define ZS_STRCASECMP zs_strcasecmp
#endif

#endif // ZS_CASE_HH
