#include <string>
#include <sstream>
#include <utility>
#include <cstdlib>
#include <iostream>
#include <cctype>
#include <functional>

#include "config.h"

#include "token.hh"
#include "test_util.hh"
#include "describe.hh"

#define PRINT_BUFSIZE 100

#define N Token::Notation

using namespace std;

template<typename T, typename Pos>
void fail_message(Token::Type t, istream& f, const Pos& b_pos,
                  const Token& tok, const T& expect){
  // extract input from stream

  f.seekg(b_pos);

  string buf;
  std::getline(f, buf);

  cerr << "[failed] input='" << buf << "', expect type='" << stringify(t) << "'"
       << ", expected str='" << expect << "'\n"
       << "\tgotten token: " << tok << '\n';

  RESULT = EXIT_FAILURE;
}


// for error case
template<>
inline
Token::Type Token::get<Token::Type>() const{
  return type_;
}

void check(istream& f){
  static const auto type = Token::Type::uninitialized;
  auto init_pos = f.tellg();

  Token tok;
  with_expect_error([&]() -> void{
      tok = tokenize(f);
    });

  if(tok.type() != type){
    fail_message(type, f, init_pos, tok, "uninitialized");
    return;
  }
}

void check(string&& input){
  stringstream ss(input);
  check(ss);
}


// for normal cases
template<typename T, typename Fun>
void check(istream& f, const T& expect,
           Fun comparator, Token::Type type){
  auto init_pos = f.tellg();

  const Token tok = tokenize(f);

  if(tok.type() != type || !comparator(tok.get<T>(),expect)){
    fail_message(type, f, init_pos, tok, expect);
    return;
  }
}

void check(istream& f, Lisp_ptr expect){
  check(f, expect, equal_internal, Token::Type::lisp_ptr);
}

void check(istream& f, N expect){
  check(f, expect, std::equal_to<N>(), Token::Type::notation);
}

template<typename T>
void check(string&& input, T&& expect){
  stringstream ss{move(input)};
  check(ss, forward<T>(expect));
}

template<typename T>
void check_ident(T&& t, const char* expect){
  string s;
  for(auto p = expect; *p; ++p){
#ifdef USE_CASE_UPPER
    s.push_back(toupper(*p));
#elif USE_CASE_LOWER
    s.push_back(tolower(*p));
#else
    s.push_back(*p);
#endif
  }

  check(forward<T>(t), intern(*vm.symtable, s));
}

template<typename T>
void check_string(T&& t, const char* expect){
  string s{expect};
  check(forward<T>(t), Lisp_ptr{&s});
}

int main(){
  zs_init();

  // identifier
  check_ident("lambda", "lambda");
  check_ident("q", "q");
  check_ident("list->vector", "list->vector");
  check_ident("soup", "soup");
  check_ident("+", "+");
  check_ident("V17a", "V17a");
  check_ident("<=?", "<=?");
  check_ident("a34kTMNs", "a34kTMNs");
  check_ident("the-word-resursin-has-many-meanings", "the-word-resursin-has-many-meanings");

  check_ident("+",  "+");
  check_ident("-",  "-");
  check_ident("...",  "...");

  check(".."); // error
  check("...."); // error

  // spaces & comments
  check_ident("   abc", "abc");
  check_ident("   abc    def ", "abc");
  check_ident(" ;hogehoge\n   abc", "abc");
  check(" ;hogehoge\n"); // EOF
  check_ident("   abc;fhei", "abc");

  // boolean
  check("#t", Lisp_ptr{true});
  check("#f", Lisp_ptr{false});

  // number
  check("+1", wrap_number(1));
  check("#x16", wrap_number(0x16));
  
  // character
  check("#\\");
  check("#\\a", Lisp_ptr{'a'});
  check("#\\b", Lisp_ptr{'b'});
  check("#\\x", Lisp_ptr{'x'});
  check("#\\s", Lisp_ptr{'s'});
  check("#\\space", Lisp_ptr{' '});
  check("#\\newline", Lisp_ptr{'\n'});

  // string
  check("\"");
  check_string("\"\"", "");
  check_string("\"a\"", "a");
  check_string("\"abcd\nedf jfkdj\"", "abcd\nedf jfkdj");
  check_string("\"\\\"\\\"\"", "\"\"");

  // error
  check("#z");


  // consecutive access
  {
    char teststr[] = "(a . b)#(c 'd) e ...;comment\nf +11 .3 `(,x ,@y \"ho()ge\")";
    stringstream ss(teststr);

    check(ss, N::l_paren);
    check_ident(ss, "a");
    check(ss, N::dot);
    check_ident(ss, "b");
    check(ss, N::r_paren);
    check(ss, N::vector_paren);
    check_ident(ss, "c");
    check(ss, N::quote);
    check_ident(ss, "d");
    check(ss, N::r_paren);
    check_ident(ss, "e");
    check_ident(ss, "...");
    check_ident(ss, "f");

    check(ss, wrap_number(11));
    check(ss, wrap_number(0.3));
    check(ss, N::quasiquote);
    check(ss, N::l_paren);
    check(ss, N::comma);
    check_ident(ss, "x");
    check(ss, N::comma_at);
    check_ident(ss, "y");
    check_string(ss, "ho()ge");
    check(ss, N::r_paren);

    check(ss); // EOF
  }

  return RESULT;
}
