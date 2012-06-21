#ifndef UTIL_HH
#define UTIL_HH

void
__attribute__((noreturn))// [[noreturn]]
unexp_default(const char*, int);

#define UNEXP_DEFAULT() unexp_default(__FILE__, __LINE__)


void
__attribute__((noreturn))// [[noreturn]]
unexp_conversion(const char*, int, const char*);

#define UNEXP_CONVERSION(to) unexp_conversion(__FILE__, __LINE__, (to))

#endif // UTIL_HH
