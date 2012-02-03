#include <string>
#include <sstream>
#include <utility>
#include <cstdlib>
#include <memory>
#include <cstdio>

#include "token.hh"

using namespace std;

static bool result;

template<typename T>
void check_copy_move(const Token& tok){
  // constructor
  {
    Token tok_c_con(tok);
    if(tok.type() != tok_c_con.type() || tok.get<T>() != tok_c_con.get<T>()){
      fputs("[failed] on copy construct: ", stdout);
      goto error;
    }else{
      Token tok_m_con{move(tok_c_con)};
      if(tok.type() != tok_m_con.type() || tok.get<T>() != tok_m_con.get<T>()){
        fputs("[failed] on move construct ", stdout);
        goto error;
      }
    }
  }
  
  // operator=
  {
    Token tok_c_op = tok;
    if(tok.type() != tok_c_op.type() || tok.get<T>() != tok_c_op.get<T>()){
      fputs("[failed] on copy operator= ", stdout);
      goto error;
    }else{
      Token tok_m_op = move(tok_c_op);
      if(tok.type() != tok_m_op.type() || tok.get<T>() != tok_m_op.get<T>()){
        fputs("[failed] on move operator= ", stdout);
        goto error;
      }
    }
  }

  return;

 error:
  result = false;
  describe(stdout, tok);
  fputc('\n', stdout);
  return;
}

template<typename Fun>
void fail_message(Token::Type t, istream& i, streampos b_pos,
                  const Token& tok, const Fun& callback){

  { // extract input from stream
    i.clear(); // clear eof

    const auto now_pos = i.tellg();
    const auto size = now_pos - b_pos + 1;
    const unique_ptr<char[]> tmp(new char[size]);

    i.seekg(b_pos);
    i.get(tmp.get(), size);

    fprintf(stdout, "[failed] input='%s', expect type='",
            tmp.get());
  }

  describe(stdout, t);
  fputc('\'', stdout);

  callback();

  fputs("\n\tgotten token: ", stdout);
  describe(stdout, tok);
  fputc('\n', stdout);

  result = false;
}

template<Token::Type type, typename Fun,
         typename ex_type = char>
void check_generic(istream& i,
                   const Fun& f){
  const auto init_pos = i.tellg();
  const Token tok = tokenize(i);

  if(tok.type() != type){
    fail_message(type, i, init_pos, tok, f);
    return;
  }
  
  check_copy_move<ex_type>(tok);
}

template<Token::Type type, typename Fun, 
         typename ex_type = typename to_type<Token::Type>::get<type>::type>
void check_generic(istream& i,
                   const ex_type& expect,
                   const Fun& f){
  const auto init_pos = i.tellg();
  const Token tok = tokenize(i);

  if(tok.type() != type || tok.get<ex_type>() != expect){
    fail_message(type, i, init_pos, tok, f);
    return;
  }
  
  check_copy_move<ex_type>(tok);
}


void check_uninit(istream& i){
  check_generic<Token::Type::uninitialized>
    (i, [](){});
}

void check_uninit(const string& input){
  stringstream is(input);
  return check_uninit(is);
}

void check_ident(istream& i, const string& expect){
  check_generic<Token::Type::identifier>
    (i, expect,
     [&](){
      fprintf(stdout, ", expected str='%s'", expect.c_str());
    });
}

void check_ident(const string& input, const string& expect){
  stringstream is(input);
  return check_ident(is, expect);
}

void check_boolean(istream& i, bool expect){
  check_generic<Token::Type::boolean>
    (i, expect, 
     [=](){
      fprintf(stdout, ", expected bool='%s'", expect ? "true" : "false");
    });
}

void check_boolean(const string& input, bool expect){
  stringstream is(input);
  return check_boolean(is, expect);
}

void check_character(istream& i, char expect){
  check_generic<Token::Type::character>
    (i, expect, 
     [=](){
      fprintf(stdout, ", expected char='%c'", expect);
    });
}

void check_character(const string& input, char expect){
  stringstream is(input);
  return check_character(is, expect);
}

void check_string(istream& i, const string& expect){
  check_generic<Token::Type::string>
    (i, expect, 
     [&](){
      fprintf(stdout, ", expected str='%s'", expect.c_str());
    });
}

void check_string(const string& input, const string& expect){
  stringstream is(input);
  return check_string(is, expect);
}

void check_notation(istream& i, Token::Notation n){
  check_generic<Token::Type::notation>
    (i, n,
     [=](){
      fputs(", expected notation='", stdout);
      describe(stdout, n);
      fputc('\'', stdout);
    });
}

void check_notation(const string& input, Token::Notation n){
  stringstream is(input);
  return check_notation(is, n);
}

int main(){
  result = true;

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
  check_uninit(".."); // error
  check_uninit("...."); // error

  // spaces & comments
  check_ident("   abc", "abc");
  check_ident("   abc    def ", "abc");
  check_ident(" ;hogehoge\n   abc", "abc");
  check_uninit(" ;hogehoge\n");
  check_ident("   abc;fhei", "abc");

  // boolean
  check_boolean("#t", true);
  check_boolean("#f", false);

  // number
  
  // character
  check_uninit("#\\");
  check_character("#\\a", 'a');
  check_character("#\\b", 'b');
  check_character("#\\x", 'x');
  check_character("#\\space", ' ');
  check_character("#\\newline", '\n');

  // string
  check_uninit("\"");
  check_string("\"\"", "");
  check_string("\"a\"", "a");
  check_string("\"abcd\nedf jfkdj\"", "abcd\nedf jfkdj");
  check_string("\"\\\"\\\"\"", "\"\"");

  // error
  check_uninit("#z");


  // consecutive access
  {
    stringstream ss("(a . b)#(c 'd) e ;... \n f `(,x ,@y \"ho()ge\")");

    check_notation(ss, Token::Notation::l_paren);
    check_ident(ss, "a");
    check_notation(ss, Token::Notation::dot);
    check_ident(ss, "b");
    check_notation(ss, Token::Notation::r_paren);
    check_notation(ss, Token::Notation::vector_paren);
    check_ident(ss, "c");
    check_notation(ss, Token::Notation::quote);
    check_ident(ss, "d");
    check_notation(ss, Token::Notation::r_paren);
    check_ident(ss, "e");

    check_ident(ss, "f");
    check_notation(ss, Token::Notation::quasiquote);
    check_notation(ss, Token::Notation::l_paren);
    check_notation(ss, Token::Notation::comma);
    check_ident(ss, "x");
    check_notation(ss, Token::Notation::comma_at);
    check_ident(ss, "y");
    check_string(ss, "ho()ge");
    check_notation(ss, Token::Notation::r_paren);

    check_uninit(ss);
  }

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

