#include <utility>
#include <istream>
#include <iostream>
#include <memory>

#include "reader.hh"
#include "vm.hh"
#include "zs_error.hh"
#include "lisp_ptr.hh"
#include "token.hh"
#include "number.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "symbol.hh"

using namespace std;

namespace {

Lisp_ptr read_la(istream& f, Token&&);

Lisp_ptr read_list(istream& f){
  Token t = tokenize(f);

  // first check
  if(!t){
    throw zs_error("reader error: reached EOF in a list.\n");
  }else if(t.type() == Token::Type::notation){
    auto n = t.get<Token::Notation>();
    if(n == Token::Notation::r_paren){ // empty list
      return Cons::NIL;
    }else if(n == Token::Notation::dot){
      throw zs_error("reader error: dotted list has no car.\n");
    }
  }

  // main loop
  GrowList gl;

  while(1){
    gl.push(read_la(f, move(t)));
    
    // check next token
    t = tokenize(f);
    if(!t){
      throw zs_error("reader error: reached EOF in a list.\n");
    }else if(t.type() == Token::Type::notation){
      auto n = t.get<Token::Notation>();
      if(n == Token::Notation::r_paren){ // proper list
        return gl.extract();
      }else if(n == Token::Notation::dot){ // dotted list
        auto ret = gl.extract_with_tail(read(f));
        t = tokenize(f);
        if(t.type() != Token::Type::notation
           || t.get<Token::Notation>() != Token::Notation::r_paren){
          throw zs_error("reader error: dotted list has two or more cdrs.\n");
        }
        return ret;
      }
    }
  }

  UNEXP_DEFAULT(); // should not come here.
}

Lisp_ptr read_vector(istream& f){
  unique_ptr<Vector> v{new Vector()};

  while(1){
    auto t = tokenize(f);
    if(!t){
      throw zs_error("reader error: reached EOF in a vector.\n");
    }else if((t.type() == Token::Type::notation)
             && (t.get<Token::Notation>()
                 == Token::Notation::r_paren)){
      return Lisp_ptr{v.release()};
    }else{
      v->emplace_back(read_la(f, move(t)));
    }
  }

  UNEXP_DEFAULT(); // should not come here.
}

Lisp_ptr read_abbrev(const char* name, istream& f){
  return make_cons_list({intern(vm.symtable(), name), read(f)});
}

Lisp_ptr read_la(istream& f, Token&& tok){
  switch(tok.type()){
    // simple datum
  case Token::Type::boolean:
    return Lisp_ptr(tok.move<bool>());

  case Token::Type::number: {
    auto n = tok.get<Number>();
    switch(n.type()){
    case Number::Type::uninitialized:
      return Lisp_ptr();
    case Number::Type::integer:
      // TODO: use upper integer type!
      return Lisp_ptr(new Number(tok.move<Number>()));
    case Number::Type::real:
      return {new double(n.get<double>())};
    case Number::Type::complex:
      return {new complex<double>(n.get<complex<double> >())};
    }
  }

  case Token::Type::character:
    return Lisp_ptr{tok.move<char>()};

  case Token::Type::string:
    return Lisp_ptr(new String(tok.move<string>()));

  case Token::Type::identifier:
    return Lisp_ptr{intern(vm.symtable(), tok.move<string>())};

    // compound datum
  case Token::Type::notation:
    switch(auto n = tok.move<Token::Notation>()){

    case Token::Notation::l_paren: // list
      return read_list(f);

    case Token::Notation::vector_paren: // vector
      return read_vector(f);

      // abbrev prefix
    case Token::Notation::quote:
      return read_abbrev("quote", f);

    case Token::Notation::quasiquote:
      return read_abbrev("quasiquote", f);

    case Token::Notation::comma:
      return read_abbrev("unquote", f);

    case Token::Notation::comma_at:
      return read_abbrev("unquote-splicing", f);
      
    case Token::Notation::l_bracket:
    case Token::Notation::l_brace:
      throw zs_error(printf_string("reader error: not supported notation! (type=%s)\n",
                                   stringify(n)));

    case Token::Notation::r_paren:
    case Token::Notation::r_bracket:
    case Token::Notation::r_brace:
      throw zs_error(printf_string("reader error: closing notation appeared alone! (type=%s)\n",
                                   stringify(n)));

    case Token::Notation::dot:
    case Token::Notation::bar:
    default:
      throw zs_error(printf_string("reader error: unexpected notation was passed! (type=%s)\n",
                                   stringify(n)));
    }

  case Token::Type::uninitialized:
    // Checking "f.peek() == EOF" is needed because tokenize() may do 'unget(EOF)'
    // TODO: remove all 'unget(EOF)' occurance.
    if(f.eof() || f.peek() == EOF){
      return Lisp_ptr{static_cast<char>(EOF)};
    }else{
      Lisp_ptr{};
    }

  default:
    UNEXP_DEFAULT();
  }
}

} // namespace

Lisp_ptr read(istream& f){
  return read_la(f, tokenize(f));
}
