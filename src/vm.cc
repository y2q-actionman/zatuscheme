#include <cassert>
#include <ostream>

#include "vm.hh"
#include "env.hh"
#include "printer.hh"

VM vm;


VM::VM() : code(), stack(),
           return_value(1, {}),
           extent(),
           frame_(new Env{nullptr}),
           symtable_(new SymTable())
{
}

VM::VM(const VM& other) : code(other.code), stack(other.stack),
                          return_value(other.return_value),
                          extent(other.extent),
                          frame_(other.frame_),
                          symtable_(other.symtable_)
{
}

VM::~VM(){
}


VM& VM::operator=(const VM& other){
  code = other.code;
  stack = other.stack;
  return_value = other.return_value;
  extent = other.extent,

  frame_ = other.frame_;
  
  symtable_ = other.symtable_;

  return *this;
}


std::ostream& operator<<(std::ostream& f, const VM& v){
  f << "--- [code] ---\n";
  for(auto i = v.code.rbegin(), e = v.code.rend(); i != e; ++i){
    print(f, *i);
    f << '\n';
  }

  f << "--- [stack] ---\n";
  for(auto i = v.stack.rbegin(), e = v.stack.rend(); i != e; ++i){
    print(f, *i);
    f << '\n';
  }

  f << "--- [return value] ---\n";
  for(auto i = v.return_value.begin(), e = v.return_value.end(); i != e; ++i){
    f << '[' << v.return_value.size() << "] ";
    print(f, *i);
    if(next(i) != e) f << ", ";
  }

  if(!v.extent.empty()){
    f << "--- [extent] ---\n";
    for(auto i = v.extent.begin(), e = v.extent.end(); i != e; ++i){
      print(f, i->thunk);
      f << ": ";

      print(f, i->before);
      f << ", ";
      print(f, i->after);
      f << "\n";
    }
  }

  // f << "--- [env] ---\n";
  // f << *v.frame_;

  // f << "--- [symtable] ---\n";
  // f << *v.symtable_;

  f << "\n\n";

  return f;
}
