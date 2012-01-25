#ifndef KEYWORD_HH
#define KEYWORD_HH

enum class Keyword{
  // error
  unknown = 0,

  // syntactic
    else_, r_arrow, define, unquote, unquote_splicing,

  // expression
    quote, lambda, if_, set_, begin,
    cond, and_, or_, case_, let,
    let_star, letrec, do_, delay, quasiquote,

  // for static_assert
    MAX
};

Keyword to_keyword(unsigned);
Keyword to_keyword(const char*);
const char* stringify(Keyword);

#endif // KEYWORD_HH
