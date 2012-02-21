#ifndef UTIL_HH
#define UTIL_HH

void
__attribute__((noreturn))// [[noreturn]]
unexp_default(const char*, int);

#define UNEXP_DEFAULT() unexp_default(__FILE__, __LINE__)


#endif // UTIL_HH
