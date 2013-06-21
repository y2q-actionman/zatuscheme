#include <utility>
#include <istream>

#include "reader.hh"
#include "vm.hh"
#include "zs_error.hh"
#include "lisp_ptr.hh"
#include "token.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "symbol.hh"
#include "rational.hh"
#include "zs_memory.hh"

using namespace std;

namespace {

Lisp_ptr read_la(istream&, Lisp_ptr);

Lisp_ptr read_list(istream& f){
  auto p = tokenize(f);

  // first check
  if(!p){
    throw zs_error("reader error: reached EOF in a list.\n");
  }else if(p.tag() == Ptr_tag::notation){
    auto n = p.get<Notation>();
    if(n == Notation::r_paren){ // empty list
      return Cons::NIL;
    }else if(n == Notation::dot){
      throw zs_error("reader error: dotted list has no car.\n");
    }
  }

  // main loop
  GrowList gl;

  while(1){
    gl.push(read_la(f, p));
    
    // check next token
    p = tokenize(f);
    if(!p){
      throw zs_error("reader error: reached EOF in a list.\n");
    }else if(p.tag() == Ptr_tag::notation){
      auto n = p.get<Notation>();
      if(n == Notation::r_paren){ // proper list
        return gl.extract();
      }else if(n == Notation::dot){ // dotted list
        auto ret = gl.extract_with_tail(read(f));
        p = tokenize(f);
        if(!p || p.get<Notation>() != Notation::r_paren){
          throw zs_error("reader error: dotted list has two or more cdrs.\n");
        }
        return ret;
      }
    }
  }

  UNEXP_DEFAULT(); // should not come here.
}

Lisp_ptr read_vector(istream& f){
  auto v = zs_new<Vector>();

  while(1){
    auto p = tokenize(f);
    if(!p){
      throw zs_error("reader error: reached EOF in a vector.\n");
    }else if(p.tag() == Ptr_tag::notation
             && p.get<Notation>() == Notation::r_paren){
      return {v};
    }else{
      v->emplace_back(read_la(f, p));
    }
  }

  UNEXP_DEFAULT(); // should not come here.
}

Lisp_ptr read_abbrev(const char* name, istream& f){
  return make_cons_list({intern(*vm.symtable, name), read(f)});
}

Lisp_ptr read_la(istream& f, Lisp_ptr p){
  if(!p){
    // Checking "f.peek() == EOF" is needed because tokenize() may do 'unget(EOF)'
    // TODO: remove all 'unget(EOF)' occurance.
    if(f.eof() || f.peek() == EOF){
      return Lisp_ptr{static_cast<char>(EOF)};
    }else{
      Lisp_ptr{};
    }
  }

  if(p.tag() != Ptr_tag::notation){
    return p;
  }

  switch(auto n = p.get<Notation>()){
  case Notation::l_paren: // list
    return read_list(f);

  case Notation::vector_paren: // vector
    return read_vector(f);

    // abbrev prefix
  case Notation::quote:
    return read_abbrev("quote", f);

  case Notation::quasiquote:
    return read_abbrev("quasiquote", f);

  case Notation::comma:
    return read_abbrev("unquote", f);

  case Notation::comma_at:
    return read_abbrev("unquote-splicing", f);
      
  case Notation::l_bracket:
  case Notation::l_brace:
    throw zs_error(printf_string("reader error: not supported notation! (type=%s)\n",
                                 stringify(n)));

  case Notation::r_paren:
  case Notation::r_bracket:
  case Notation::r_brace:
    throw zs_error(printf_string("reader error: closing notation appeared alone! (type=%s)\n",
                                 stringify(n)));

  case Notation::dot:
  case Notation::bar:
  default:
    throw zs_error(printf_string("reader error: unexpected notation was passed! (type=%s)\n",
                                 stringify(n)));
  }
}

} // namespace

Lisp_ptr read(istream& f){
  return read_la(f, tokenize(f));
}

bool eof_object_p(Lisp_ptr p){
  return (p.tag() == Ptr_tag::character)
    && (p.get<char>() == EOF);
}
