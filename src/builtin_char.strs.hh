// This file is intended to be included into an array of 'const char*'

#define CHAR_CMP_FUNCS(op)                              \
  "(define (char"op"? c1 c2)"                           \
  "  ("op" (char->integer c1) (char->integer c2)))"
CHAR_CMP_FUNCS("="),
CHAR_CMP_FUNCS("<"),
CHAR_CMP_FUNCS(">"),
CHAR_CMP_FUNCS("<="),
CHAR_CMP_FUNCS(">="),
#undef CHAR_CMP_FUNCS

#define CHAR_CI_CMP_FUNCS(op)                           \
  "(define (char-ci"op"? c1 c2)"                        \
  "  ("op" (%char-casecmp c1 c2) 0))"
CHAR_CI_CMP_FUNCS("="),
CHAR_CI_CMP_FUNCS("<"),
CHAR_CI_CMP_FUNCS(">"),
CHAR_CI_CMP_FUNCS("<="),
CHAR_CI_CMP_FUNCS(">="),
#undef CHAR_CI_CMP_FUNCS
