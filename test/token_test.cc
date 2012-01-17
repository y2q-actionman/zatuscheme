#include <string>
#include <sstream>
#include <iostream>
#include <utility>
#include <cstdlib>

#include "token.hh"

using namespace std;

static bool result;

void describe(const Token& tok){
  clog << "Type=" << static_cast<int>(tok.type());
  
  switch(tok.type()){
  case Token::Type::uninitialized:
    break;
  case Token::Type::number:
    // not implemented
    break;
  case Token::Type::boolean:
    clog << " bool=" << tok.boolean();
    break;
  default:
    clog << " str=" << tok.str();
  }
}

void check(const string& input, Token::Type typ, const string& expect){
  stringstream is(input);
  Token tok = tokenize(is);

  if(tok.type() != typ || tok.str() != expect){
    clog << "failed: input=" << input
         << " , expect type=" << static_cast<int>(typ)
         << " , expect str=" << expect
         << '\n';

    clog << "  gotten token: ";
    describe(tok);
    clog << endl;

    result = false;
    return;
  }

  // constructor
  Token tok_c_con(tok);
  if(tok.type() != tok_c_con.type() || tok.str() != tok_c_con.str()){
    cout << "failed on copy construct (input=" << input << ")" << endl;
    result = false;
    return;
  }

  Token tok_m_con{move(tok_c_con)};
  if(tok.type() != tok_m_con.type() || tok.str() != tok_m_con.str()){
    cout << "failed on move construct (input=" << input << ")" << endl;
    result = false;
    return;
  }
  
  // operator=
  Token tok_c_op = tok;
  if(tok.type() != tok_c_op.type() || tok.str() != tok_c_op.str()){
    cout << "failed on copy operator= (input=" << input << ")" << endl;
    result = false;
    return;
  }

  Token tok_m_op = std::move(tok_c_op);
  if(tok.type() != tok_m_op.type() || tok.str() != tok_m_op.str()){
    cout << "failed on move operator= (input=" << input << ")" << endl;
    result = false;
    return;
  }
  
  return;
}

int main(){
  result = true;

  // identifier
  check("lambda", Token::Type::identifier, "lambda");
  check("q", Token::Type::identifier, "q");
  check("list->vector", Token::Type::identifier, "list->vector");
  check("soup", Token::Type::identifier, "soup");
  check("+", Token::Type::identifier, "+");
  check("V17a", Token::Type::identifier, "V17a");
  check("<=?", Token::Type::identifier, "<=?");
  check("a34kTMNs", Token::Type::identifier, "a34kTMNs");
  check("the-word-resursin-has-many-meanings", Token::Type::identifier,
        "the-word-resursin-has-many-meanings");

  check("+", Token::Type::identifier, "+");
  check("-", Token::Type::identifier, "-");
  check("...", Token::Type::identifier, "...");

  check("..", Token::Type::uninitialized, "");


  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

