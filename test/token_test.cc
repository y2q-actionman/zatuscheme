#include <string>
#include <sstream>
#include <iostream>
#include <utility>
#include <cstdlib>
#include <memory>

#include "token.hh"

using namespace std;

static bool result;

template<typename T>
void check_copy_move(const Token& tok){
  // constructor
  {
    Token tok_c_con(tok);
    if(tok.type() != tok_c_con.type() || tok.get<T>() != tok_c_con.get<T>()){
      cout << "[failed] on copy construct: ";
      goto error;
    }else{
      Token tok_m_con{move(tok_c_con)};
      if(tok.type() != tok_m_con.type() || tok.get<T>() != tok_m_con.get<T>()){
        cout << "[failed] on move construct ";
        goto error;
      }
    }
  }
  
  // operator=
  {
    Token tok_c_op = tok;
    if(tok.type() != tok_c_op.type() || tok.get<T>() != tok_c_op.get<T>()){
      cout << "[failed] on copy operator= ";
      goto error;
    }else{
      Token tok_m_op = move(tok_c_op);
      if(tok.type() != tok_m_op.type() || tok.get<T>() != tok_m_op.get<T>()){
        cout << "[failed] on move operator= ";
        goto error;
      }
    }
  }

  return;

 error:
  result = false;
  describe(cout, tok);
  cout << endl;
  return;
}

template<typename Fun>
void fail_message(Token::Type t, istream& i, streampos b_pos,
                  const Token& tok, const Fun& callback){
  clog << "[failed] input='";

  { // extract input from stream
    i.clear(); // clear eof

    const auto now_pos = i.tellg();
    const auto size = now_pos - b_pos + 1;
    const unique_ptr<char[]> tmp(new char[size]);

    i.seekg(b_pos);
    i.get(tmp.get(), size);

    clog << tmp.get();
  }

  clog << "', expect type='";
  describe(clog, t);
  clog << "'";

  callback();

  clog << '\n';

  clog << "\tgotten token: ";
  describe(clog, tok);
  clog << endl;

  result = false;
}

void check_uninit(istream& i){
  const auto init_pos = i.tellg();
  const Token tok = tokenize(i);

  if(tok.type() != Token::Type::uninitialized){
    fail_message(Token::Type::uninitialized, i, init_pos, tok,
                 [](){});
    return;
  }
  
  check_copy_move<char>(tok);
}

void check_uninit(const string& input){
  stringstream is(input);
  return check_uninit(is);
}

void check_ident(istream& i, const string& expect){
  const auto init_pos = i.tellg();
  const Token tok = tokenize(i);

  if(tok.type() != Token::Type::identifier || tok.get<string>() != expect){
    fail_message(Token::Type::identifier, i, init_pos, tok,
                 [&](){
                   clog << ", expect str='" << expect << "'";
                 });
    return;
  }
  
  check_copy_move<string>(tok);
}

void check_ident(const string& input, const string& expect){
  stringstream is(input);
  return check_ident(is, expect);
}

void check_notation(istream& i, Token::Notation n){
  const auto init_pos = i.tellg();
  const Token tok = tokenize(i);

  if(tok.type() != Token::Type::notation ||
     tok.get<Token::Notation>() != n){
    fail_message(Token::Type::notation, i, init_pos, tok,
                 [&](){
                   clog << ", expect notation='";
                   describe(clog, n);
                   clog << "'";
                 });
    return;
  }
  
  check_copy_move<string>(tok);
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

  // spaces & comments
  check_ident("   abc", "abc");
  check_ident("   abc    def ", "abc");
  check_ident(" ;hogehoge\n   abc", "abc");
  check_uninit(" ;hogehoge\n");
  check_ident("   abc;fhei", "abc");

  // notation
  check_notation(".", Token::Notation::dot);

  // error
  check_uninit("..");
  check_uninit("....");


  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

