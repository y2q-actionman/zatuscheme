#include "keyword.hh"

#include <cstring>
#include <array>
#include <algorithm>

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
  auto ret = find_if(keyword_table.begin(), keyword_table.end(),
                     [=](const Entry& e){ 
                       return strcmp(e.str, s) == 0;
                     });

  return (ret != keyword_table.end())
    ? ret->k : Keyword::unknown;
}

const char* stringify(Keyword k){
  auto ret = find_if(keyword_table.begin(), keyword_table.end(),
                     [=](const Entry& e){ 
                       return e.k == k;
                     });

  return (ret != keyword_table.end())
    ? ret->str : nullptr;
}
