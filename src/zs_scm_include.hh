#ifndef ZS_SCM_INCLUDE_HH
#define ZS_SCM_INCLUDE_HH

#define LOAD(...) "("#__VA_ARGS__")"

/*
LOAD allows to include a Scheme code as C string.
Example:
  LOAD(define x 100) -> "(define x 100)"

Restrictions:
  - every form must begin with '('.

  - ' (quote) causes error. Use (quote...) instead.

  - cannot use notations begins with '#' (#t, #f, etc.).
*/

#endif // ZS_SCM_INCLUDE_HH
