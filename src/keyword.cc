#include "keyword.hh"

#include <array>
#include <cassert>

using namespace std;

namespace {

struct Entry {
  Keyword k;
  const char* str;
};

static constexpr
array<Entry, static_cast<int>(Keyword::MAX)>
keyword_table{{
    { Keyword::not_keyword, "\0" },

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

const char* stringify(Keyword k){
  auto& e = keyword_table.at(static_cast<int>(k));
  assert(e.k == k);
  return e.str;
}

Keyword to_keyword(const std::string& s){
  for(const auto& e : keyword_table){
    if(s == e.str)
      return e.k;
  }

  return Keyword::not_keyword;
}

