#include <string>
#include <sstream>
#include <utility>
#include <cstdlib>
#include <iostream>

#include "token.hh"
#include "test_util.hh"
#include "describe.hh"

#define PRINT_BUFSIZE 100

using namespace std;

template<typename T>
void check_copy_move(const Token& tok){
  // constructor
  {
    Token tok_c_con(tok);
    if(tok.type() != tok_c_con.type() || tok.get<T>() != tok_c_con.get<T>()){
      cerr << "[failed] on copy construct: ";
      goto error;
    }else{
      Token tok_m_con{move(tok_c_con)};
      if(tok.type() != tok_m_con.type() || tok.get<T>() != tok_m_con.get<T>()){
        cerr << "[failed] on move construct ";
        goto error;
      }
    }
  }
  
  // operator=
  {
    Token tok_c_op = tok;
    if(tok.type() != tok_c_op.type() || tok.get<T>() != tok_c_op.get<T>()){
      cerr << "[failed] on copy operator= ";
      goto error;
    }else{
      Token tok_m_op = move(tok_c_op);
      if(tok.type() != tok_m_op.type() || tok.get<T>() != tok_m_op.get<T>()){
        cerr << "[failed] on move operator= ";
        goto error;
      }
    }
  }

  return;

 error:
  RESULT = EXIT_FAILURE;
  cerr << tok << '\n';
  return;
}

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
  
  check_copy_move<Token::Type>(tok);
}

void check(string&& input){
  stringstream ss(input);
  check(ss);
}


// for normal cases
template<typename T>
void check(istream& f, const T& expect,
           Token::Type type = to_tag<Token::Type, T>()){
  auto init_pos = f.tellg();

  const Token tok = tokenize(f);

  if(tok.type() != type || tok.get<T>() != expect){
    fail_message(type, f, init_pos, tok, expect);
    return;
  }
  
  check_copy_move<T>(tok);
}

template<typename T>
void check(string&& input, T&& expect){
  stringstream ss(input);
  check(ss, expect);
}

void check(istream& f, const char* expect, Token::Type t){
  check(f, std::string(expect), t);
}

void check(const string& input, const char* expect, Token::Type t){
  stringstream ss(input);
  check(ss, expect, t);
}


#define check_ident(a, b) check(a, b, Token::Type::identifier)
#define check_string(a, b) check(a, b, Token::Type::string)

#define N Token::Notation

int main(){
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
  check("#t", true);
  check("#f", false);

  // number
  check("+1", 1);
  check("#x16", 0x16);
  
  // character
  check("#\\");
  check("#\\a", 'a');
  check("#\\b", 'b');
  check("#\\x", 'x');
  check("#\\s", 's');
  check("#\\space", ' ');
  check("#\\newline", '\n');

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
    check(ss, 11);
    check(ss, 0.3);
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
