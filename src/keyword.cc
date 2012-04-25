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
array<Entry, static_cast<int>(Keyword::MAX)+1>
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
    { Keyword::letrec, "letrec" },
    { Keyword::do_, "do" },
    { Keyword::delay, "delay" },
    { Keyword::quasiquote, "quasiquote" },

    { Keyword::MAX, nullptr }
}};

} // namespace

const char* stringify(Keyword k){
  auto& e = keyword_table.at(static_cast<int>(k));
  assert(e.k == k);
  return e.str;
}

Keyword to_keyword(const std::string& s){
#if 0
  for(const auto& e : keyword_table){
    if(s == e.str)
      return e.k;
  }

  return Keyword::not_keyword;
#else
  if(s.empty())
    return Keyword::not_keyword;

  Keyword ret = Keyword::not_keyword;

  switch(s[0]){
  case '=':
    if(s.compare(1, string::npos, ">") == 0)
      ret = Keyword::r_arrow;
    break;

  case 'a':
    if(s.compare(1, string::npos, "nd") == 0)
      ret = Keyword::and_;
    break;

  case 'b':
    if(s.compare(1, string::npos, "egin") == 0)
      ret = Keyword::begin;
    break;

  case 'c':
    if(s.length() != 4) break;
    switch(s[1]){
    case 'a':
      if(s.compare(2, string::npos, "se") == 0)
        ret = Keyword::case_;
      break;
    case 'o':
      if(s.compare(2, string::npos, "nd") == 0)
        ret = Keyword::cond;
      break;
    }
    break;

  case 'd':
    if(s.length() < 2) break;
    switch(s[1]){
    case 'e':
      if(s.length() < 5) break;
      switch(s[2]){
      case 'f':
        if(s.compare(3, string::npos, "ine") == 0)
          ret = Keyword::define;
        break;
      case 'l':
        if(s.compare(3, string::npos, "ay") == 0)
          ret = Keyword::delay;
        break;
      }
      break;
    case 'o':
      if(s.length() == 2)
        ret = Keyword::do_;
      break;
    }
    break;

  case 'e':
    if(s.compare(1, string::npos, "lse") == 0)
      ret = Keyword::else_;
    break;

  case 'i':
    if(s.compare(1, string::npos, "f") == 0)
      ret = Keyword::if_;
    break;

  case 'l':
    if(s.length() < 3) break;
    switch(s[1]){
    case 'a':
      if(s.compare(2, string::npos, "mbda") == 0)
        ret = Keyword::lambda;
      break;
    case 'e':
      if(s[2] != 't') break;
      if(s.length() == 3)
        ret = Keyword::let;
      else if(s.compare(3, string::npos, "*") == 0)
        ret = Keyword::let_star;
      else if(s.compare(3, string::npos, "rec") == 0)
        ret = Keyword::letrec;
      break;
    }
    break;

  case 'o':
    if(s.compare(1, string::npos, "r") == 0)
      ret = Keyword::or_;
    break;

  case 'q':
    if(s.length() < 5 || s[1] != 'u') break;
    switch(s[2]){
    case 'a':
      if(s.compare(3, string::npos, "siquote") == 0)
        ret = Keyword::quasiquote;
      break;
    case 'o':
      if(s.compare(3, string::npos, "te") == 0)
        ret = Keyword::quote;
      break;
    }
    break;

  case 's':
    if(s.compare(1, string::npos, "et!") == 0)
      ret = Keyword::set_;
    break;

  case 'u': {
    auto cmp = s.compare(1, 7, "nquote");
    if(cmp == 0){
      ret = Keyword::unquote;
    }else if(cmp > 0 && 
             s.compare(7, string::npos, "-splicing") == 0)
      ret = Keyword::unquote_splicing;
    }
    break;
  }

  return ret;
#endif
}
