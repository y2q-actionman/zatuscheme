#include <exception>
#include <ostream>

#include "printer.hh"
#include "vm.hh"
#include "zs_memory.hh"

namespace zs {

VM vm;

VM::VM() : code(), stack(),
           return_value(1, {}),
           extent(),
           symtable(),
           frame(nullptr),
           exception_handler(),
	   name()
{}

VM::VM(const VM&) = default;

VM::~VM() = default;

VM& VM::operator=(const VM&) = default;

Lisp_ptr VM::return_value_1(){
  return (return_value.empty()) ? Lisp_ptr{} : return_value[0];
}

std::ostream& operator<<(std::ostream& f, const VM& v){
  f << "--- [code] ---\n";
  for(auto i = v.code.rbegin(), e = v.code.rend(); i != e; ++i){
    f << *i << '\n';
  }

  f << "--- [stack] ---\n";
  for(auto i = v.stack.rbegin(), e = v.stack.rend(); i != e; ++i){
    f << *i << '\n';
  }

  f << "--- [return value] ---\n";
  f << '[' << v.return_value.size() << "] ";
  for(auto i = v.return_value.begin(), e = v.return_value.end(); i != e; ++i){
    f << '\t' << *i << '\n';
  }

  if(!v.extent.empty()){
    f << "--- [extent] ---\n";
    for(auto i = v.extent.begin(), e = v.extent.end(); i != e; ++i){
      f << i->thunk << ": " << i->before << ", " << i->after << "\n";
    }
  }

  // f << "--- [env] ---\n";
  // f << *v.frame_;

  // f << "--- [symtable] ---\n";
  // f << *v.symtable_;

  if(!v.exception_handler.empty()){
    f << "--- [exception handler] ---\n";
    for(auto i = v.exception_handler.rbegin(), e = v.exception_handler.rend(); i != e; ++i){
      f << *i << "\n";
    }
  }

  f << "\n\n";

  return f;
}


// class ZsArgs
// - invalidate() :: marks as unusable.
// - cleanup() :: destroys vm's arguments really.

void ZsArgs::cleanup(){
  if(size_ < 0) return;

  if(!std::uncaught_exception()){
    vm.stack.erase(this->begin(), this->end() + 1);
    invalidate();
  }
}

} // namespace zs
