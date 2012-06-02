#include <cstring>

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

Keyword to_keyword(const char* s){
  if(!s) return Keyword::not_keyword;

  switch(s[0]){
  case '=':
    if(strcmp(s+1, ">") == 0)
      return Keyword::r_arrow;
    break;

  case 'a':
    if(strcmp(s+1, "nd") == 0)
      return Keyword::and_;
    break;

  case 'b':
    if(strcmp(s+1, "egin") == 0)
      return Keyword::begin;
    break;

  case 'c':
    switch(s[1]){
    case 'a':
      if(strcmp(s+2, "se") == 0)
        return Keyword::case_;
      break;
    case 'o':
      if(strcmp(s+2, "nd") == 0)
        return Keyword::cond;
      break;
    }
    break;

  case 'd':
    switch(s[1]){
    case 'e':
      switch(s[2]){
      case 'f':
        if(strcmp(s+3, "ine") == 0)
          return Keyword::define;
        break;
      case 'l':
        if(strcmp(s+3, "ay") == 0)
          return Keyword::delay;
        break;
      }
      break;
    case 'o':
      if(s[2] == '\0')
        return Keyword::do_;
      break;
    }
    break;

  case 'e':
    if(strcmp(s+1, "lse") == 0)
      return Keyword::else_;
    break;

  case 'i':
    if(strcmp(s+1, "f") == 0)
      return Keyword::if_;
    break;

  case 'l':
    switch(s[1]){
    case 'a':
      if(strcmp(s+2, "mbda") == 0)
        return Keyword::lambda;
      break;
    case 'e':
      if(s[2] != 't') break;

      switch(s[3]){
      case '\0':
        return Keyword::let;
      case '*':
        if(s[4] == '\0')
          return Keyword::let_star;
        break;
      case 'r':
        if(strcmp(s+4, "ec") == 0)
          return Keyword::letrec;
        break;
      }
      break;
    }
    break;

  case 'o':
    if(strcmp(s+1, "r") == 0)
      return Keyword::or_;
    break;

  case 'q':
    if(s[1] != 'u') break;
    switch(s[2]){
    case 'a':
      if(strcmp(s+3, "siquote") == 0)
        return Keyword::quasiquote;
      break;
    case 'o':
      if(strcmp(s+3, "te") == 0)
        return Keyword::quote;
      break;
    }
    break;

  case 's':
    if(strcmp(s+1, "et!") == 0)
      return Keyword::set_;
    break;

  case 'u':
    if(strncmp(s+1, "nquote", sizeof("nquote")-1) == 0){
      switch(s[7]){
      case '\0':
        return Keyword::unquote;
      case '-':
        if(strcmp(s+8, "splicing") == 0)
          return Keyword::unquote_splicing;
        break;
      }
    }
    break;
  }

  return Keyword::not_keyword;
}
