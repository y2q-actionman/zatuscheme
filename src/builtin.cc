#include <array>

#include "builtin.hh"
#include "number.hh"
#include "function.hh"
#include "lisp_ptr.hh"

using namespace std;

namespace {

template<int i>
array<Lisp_ptr, i> pick_args(){
  auto ret = array<Lisp_ptr, i>();

  for(auto it = ret.rbegin(); it != ret.rend(); ++it){
    *it = VM.stack().top();
    VM.stack().pop();
  }

  return ret;
}


Lisp_ptr plus_2(){
  auto args = pick_args<3>();

  Number* n1 = args[0].get<Number*>();
  if(!n1){
    fprintf(stderr, "native func '+': first arg is not number! %s\n",
            stringify(args[0].tag()));
    return {};
  }

  Number* n2 = args[1].get<Number*>();
  if(!n2){
    fprintf(stderr, "native func '+': second arg is not number! %s\n",
            stringify(args[1].tag()));
    return {};
  }

  Number* newn = new Number(n1->get<long>() + n2->get<long>());
  return Lisp_ptr(newn);
}




struct Entry {
  const char* name;
  Function func;
};

static Entry builtin_func[] = {
  {"+", Function{plus_2, {Lisp_ptr{}, 2, true}}}
};

} // namespace

void install_builtin(){
  for(auto& e : builtin_func){
    Symbol* s = VM.symtable.intern(e.name);
    VM.set(s, Lisp_ptr{&e.func});
  }
}

bool eq(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() != b.tag()) return false;

  switch(a.tag()){
  case Ptr_tag::boolean:
    return a.get<bool>() == b.get<bool>();
  case Ptr_tag::character:
    return a.get<char>() == b.get<char>();
  default:
    return a.get<void*>() == b.get<void*>();
  }
}

bool eql(Lisp_ptr a, Lisp_ptr b){
  if(eq(a, b)) return true;

  if(a.tag() == Ptr_tag::number && b.tag() == Ptr_tag::number){
    return eql(*a.get<Number*>(), *b.get<Number*>());
  }

  return false;
}
