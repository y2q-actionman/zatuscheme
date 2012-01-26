#include "symbol.hh"

#include <cstring>
#include <array>

using namespace std;

namespace {

struct Entry {
  Symbol::Keyword k;
  const char* str;
};

constexpr
array<Entry, static_cast<int>(Symbol::Keyword::MAX)>
keyword_table{{
    { Symbol::Keyword::not_keyword, "" },

    { Symbol::Keyword::else_, "else" },
    { Symbol::Keyword::r_arrow, "=>" },
    { Symbol::Keyword::define, "define" },
    { Symbol::Keyword::unquote, "unquote" },
    { Symbol::Keyword::unquote_splicing, "unquote-splicing" },

    { Symbol::Keyword::quote, "quote" },
    { Symbol::Keyword::lambda, "lambda" },
    { Symbol::Keyword::if_, "if" },
    { Symbol::Keyword::set_, "set!" },
    { Symbol::Keyword::begin, "begin" },
    { Symbol::Keyword::cond, "cond" },
    { Symbol::Keyword::and_, "and" },
    { Symbol::Keyword::or_, "or" },
    { Symbol::Keyword::case_, "case" },
    { Symbol::Keyword::let, "let" },
    { Symbol::Keyword::let_star, "let*" },
    { Symbol::Keyword::letrec, "ketrec" },
    { Symbol::Keyword::do_, "do" },
    { Symbol::Keyword::delay, "delay" },
    { Symbol::Keyword::quasiquote, "quasiquote" }
}};

const char* stringify(Symbol::Keyword k){
  for(const auto& e : keyword_table){
    if(e.k == k)
      return e.str;
  }

  return nullptr;
}

} // namespace

Symbol::Symbol(Symbol::Keyword k)
  : name_(stringify(k)){}

Symbol::Keyword Symbol::keyword() const{
  for(const auto& e : keyword_table){
    if(strcmp(e.str, name_) == 0)
      return e.k;
  }

  return Symbol::Keyword::not_keyword;
}

