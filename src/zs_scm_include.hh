#ifndef ZS_SCM_INCLUDE_HH
#define ZS_SCM_INCLUDE_HH

#define STRINGIFY(...) #__VA_ARGS__
#define EXPAND_STRINGIFY(...) STRINGIFY(__VA_ARGS__)

/*
LOAD allows to include a Scheme code as C string.
Example:
  LOAD(define x 100) -> "(define x 100)"

Notes:
  - every form must begin with '('.

  - ' (quote) causes error. Use (quote...) instead.

  - '#' notations may be confused with CPP directives.
    Do not begin a line with '#'.

  - If not in literal, '\' (backslash) is reduced.
*/
#define LOAD(...)	"(" EXPAND_STRINGIFY(__VA_ARGS__) ")"

#define NEWLINE_CHAR #\\newline

#endif // ZS_SCM_INCLUDE_HH
