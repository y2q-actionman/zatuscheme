#include "keyword.hh"
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cassert>

using namespace std;

static constexpr size_t MAX_KEYWORD_LENGTH = sizeof("unquote-splicing");

static bool result = true;

static void check(Keyword k, const char* name){
  const auto len = strlen(name);
  assert(len < MAX_KEYWORD_LENGTH);

  if(strcmp(stringify(k), name) != 0 || to_keyword(name) != k){
    printf("[failed] keyword '%d' <==> string '%s'\n",
           static_cast<int>(k), name);
    result = false;
  }

  char buf[MAX_KEYWORD_LENGTH + 1];
  strncpy(buf, name, sizeof(buf));

  buf[len] = '_';
  if(to_keyword(buf) == k){
    printf("[failed] string '%s' => keyword '%d' unexpectedly\n",
           name, static_cast<int>(k));
    result = false;
  }

  if(len >= 2){
    buf[len-2] = '\0';
    if(to_keyword(buf) == k){
      printf("[failed] string '%s' => keyword '%d' unexpectedly\n",
             name, static_cast<int>(k));
      result = false;
    }
  }
}

int main(){
  check(Keyword::else_, "else");
  check(Keyword::r_arrow, "=>");
  check(Keyword::define, "define");
  check(Keyword::unquote, "unquote");
  check(Keyword::unquote_splicing, "unquote-splicing");

  check(Keyword::quote, "quote");
  check(Keyword::lambda, "lambda");
  check(Keyword::if_, "if");
  check(Keyword::set_, "set!");
  check(Keyword::begin, "begin");
  check(Keyword::cond, "cond");
  check(Keyword::and_, "and");
  check(Keyword::or_, "or");
  check(Keyword::case_, "case");
  check(Keyword::let, "let");
  check(Keyword::let_star, "let*");
  check(Keyword::letrec, "letrec");
  check(Keyword::do_, "do");
  check(Keyword::delay, "delay");
  check(Keyword::quasiquote, "quasiquote");

  stringify(Keyword::not_keyword);
  stringify(Keyword::MAX);

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
