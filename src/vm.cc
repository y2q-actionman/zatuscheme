#include <cassert>
#include <ostream>
#include <exception>

#include "vm.hh"
#include "env.hh"
#include "printer.hh"
#include "zs_memory.hh"

VM vm;

VM::VM() : code(), stack(),
           return_value(1, {}),
           extent(),
           symtable(),
           frame(nullptr),
           exception_handler()
{}

VM::VM(const VM& other) : code(other.code), stack(other.stack),
                          return_value(other.return_value),
                          extent(other.extent),
                          symtable(other.symtable),
                          frame(other.frame),
                          exception_handler(other.exception_handler)
{}

VM::~VM(){}

VM& VM::operator=(const VM& other){
  code = other.code;
  stack = other.stack;
  return_value = other.return_value;
  extent = other.extent,
  symtable = other.symtable;
  frame = other.frame;
  exception_handler = other.exception_handler;
  return *this;
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

ZsArgs::ZsArgs()
  : size_(vm.stack.back().get<int>()),
    stack_iter_s_(vm.stack.end() - (size_ + 1)){}

ZsArgs::ZsArgs(ZsArgs&& other)
  : size_(other.size_),
    stack_iter_s_(move(other.stack_iter_s_)){
  other.invalidate();
}

ZsArgs::~ZsArgs(){
  cleanup();
}

ZsArgs& ZsArgs::operator=(ZsArgs&& other){
  size_ = other.size_;
  stack_iter_s_ = move(other.stack_iter_s_);
  other.invalidate();
  return *this;
}

void ZsArgs::cleanup(){
  if(size_ < 0) return;

  if(!std::uncaught_exception()){
    vm.stack.erase(this->begin(), this->end() + 1);
    invalidate();
  }
}

inline
void ZsArgs::invalidate(){
  size_ = -1;
}

inline
bool ZsArgs::valid() const{
  return (size_ >= 0);
}
