#ifndef KEYWORD_HH
#define KEYWORD_HH

#include <string>

enum class Keyword{
  not_keyword = 0,

  // syntactic
    else_, r_arrow, define, unquote, unquote_splicing,

  // expression
    quote, lambda, if_, set_, begin,
    cond, and_, or_, case_, let,
    let_star, letrec, do_, delay, quasiquote,

    MAX
    };

const char* stringify(Keyword);
Keyword to_keyword(const std::string&);

#endif // KEYWORD_HH
