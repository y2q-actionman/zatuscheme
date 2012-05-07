#include "keyword.hh"

using namespace std;

const char* stringify(Keyword k){
  switch(k){
  case Keyword::else_:
    return "else";
  case Keyword::r_arrow:
    return "=>";
  case Keyword::define:
    return "define";
  case Keyword::unquote:
    return "unquote";
  case Keyword::unquote_splicing:
    return "unquote-splicing";
  case Keyword::quote:
    return "quote";
  case Keyword::lambda:
    return "lambda";
  case Keyword::if_:
    return "if";
  case Keyword::set_:
    return "set!";
  case Keyword::begin:
    return "begin";
  case Keyword::cond:
    return "cond";
  case Keyword::and_:
    return "and";
  case Keyword::or_:
    return "or";
  case Keyword::case_:
    return "case";
  case Keyword::let:
    return "let";
  case Keyword::let_star:
    return "let*";
  case Keyword::letrec:
    return "letrec";
  case Keyword::do_:
    return "do";
  case Keyword::delay:
    return "delay";
  case Keyword::quasiquote:
    return "quasiquote";
  default:
    return "(unknown keyword type)";
  }
}

Keyword to_keyword(const std::string& s){
  if(s.empty())
    return Keyword::not_keyword;

  switch(s[0]){
  case '=':
    if(s.compare(1, string::npos, ">") == 0)
      return Keyword::r_arrow;
    break;

  case 'a':
    if(s.compare(1, string::npos, "nd") == 0)
      return Keyword::and_;
    break;

  case 'b':
    if(s.compare(1, string::npos, "egin") == 0)
      return Keyword::begin;
    break;

  case 'c':
    if(s.length() != 4) break;
    switch(s[1]){
    case 'a':
      if(s.compare(2, string::npos, "se") == 0)
        return Keyword::case_;
      break;
    case 'o':
      if(s.compare(2, string::npos, "nd") == 0)
        return Keyword::cond;
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
          return Keyword::define;
        break;
      case 'l':
        if(s.compare(3, string::npos, "ay") == 0)
          return Keyword::delay;
        break;
      }
      break;
    case 'o':
      if(s.length() == 2)
        return Keyword::do_;
      break;
    }
    break;

  case 'e':
    if(s.compare(1, string::npos, "lse") == 0)
      return Keyword::else_;
    break;

  case 'i':
    if(s.compare(1, string::npos, "f") == 0)
      return Keyword::if_;
    break;

  case 'l':
    if(s.length() < 3) break;
    switch(s[1]){
    case 'a':
      if(s.compare(2, string::npos, "mbda") == 0)
        return Keyword::lambda;
      break;
    case 'e':
      if(s[2] != 't') break;
      if(s.length() == 3)
        return Keyword::let;
      else if(s.compare(3, string::npos, "*") == 0)
        return Keyword::let_star;
      else if(s.compare(3, string::npos, "rec") == 0)
        return Keyword::letrec;
      break;
    }
    break;

  case 'o':
    if(s.compare(1, string::npos, "r") == 0)
      return Keyword::or_;
    break;

  case 'q':
    if(s.length() < 5 || s[1] != 'u') break;
    switch(s[2]){
    case 'a':
      if(s.compare(3, string::npos, "siquote") == 0)
        return Keyword::quasiquote;
      break;
    case 'o':
      if(s.compare(3, string::npos, "te") == 0)
        return Keyword::quote;
      break;
    }
    break;

  case 's':
    if(s.compare(1, string::npos, "et!") == 0)
      return Keyword::set_;
    break;

  case 'u': {
    auto cmp = s.compare(1, 7, "nquote");
    if(cmp == 0){
      return Keyword::unquote;
    }else if(cmp > 0 && 
             s.compare(7, string::npos, "-splicing") == 0)
      return Keyword::unquote_splicing;
    }
    break;
  }

  return Keyword::not_keyword;
}
