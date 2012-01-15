#include <string>
#include <sstream>
#include <iostream>
#include <utility>
#include <cstdlib>

#include "token.hh"

using namespace std;

static bool result;

ostream& operator<<(ostream& o, Token::Type t){
  o << static_cast<int>(t);
  return o;
}

void check(const string& input, Token::Type typ, const string& expect){
  stringstream is(input);
  Token tok = tokenize(is);

  if(tok.type() != typ || tok.str() != expect){
    cout << "failed: input=" << input
         << " , expect type=" << typ
         << " , expect str=" << expect
         << '\n';

    cout << "        got type=" << tok.type()
         << " , got str=" << tok.str()
         << endl;
    
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


  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}


  
