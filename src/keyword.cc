#include "keyword.hh"

#include <cstring>
#include <array>

using namespace std;

namespace {

struct Entry {
  Keyword k;
  const char* str;
};

constexpr
array<Entry, static_cast<int>(Keyword::MAX)>
keyword_table{{
    { Keyword::unknown, "" },

    { Keyword::else_, "else" },
    { Keyword::r_arrow, "=>" },
    { Keyword::define, "define" },
    { Keyword::unquote, "unquote" },
    { Keyword::unquote_splicing, "unquote-splicing" },

    { Keyword::quote, "quote" },
    { Keyword::lambda, "lambda" },
    { Keyword::if_, "if" },
    { Keyword::set_, "set!" },
    { Keyword::begin, "begin" },
    { Keyword::cond, "cond" },
    { Keyword::and_, "and" },
    { Keyword::or_, "or" },
    { Keyword::case_, "case" },
    { Keyword::let, "let" },
    { Keyword::let_star, "let*" },
    { Keyword::letrec, "ketrec" },
    { Keyword::do_, "do" },
    { Keyword::delay, "delay" },
    { Keyword::quasiquote, "quasiquote" }
}};

} // namespace

Keyword to_keyword(const char* s){
  for(const auto& e : keyword_table){
    if(strcmp(e.str, s) == 0)
      return e.k;
  }

  return Keyword::unknown;
}

const char* stringify(Keyword k){
  for(const auto& e : keyword_table){
    if(e.k == k)
      return e.str;
  }

  return nullptr;
}
