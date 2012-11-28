#include <cassert>
#include <ostream>

#include "vm.hh"
#include "env.hh"
#include "printer.hh"

VM vm;

namespace {

template<typename T>
void add_ref(T& frame_seq){
  for(auto e : frame_seq){
    add_ref(e);
  }
}

template<typename T>
void release(T& frame_seq){
  for(auto e : frame_seq){
    release(e);
  }
}

} // namespace


VM::VM() : code(), stack(),
           return_value(1, {}),
           extent(),
           frames_(),
           symtable_(new SymTable())
{
  enter_frame(new Env{nullptr});
}

VM::VM(const VM& other) : code(other.code), stack(other.stack),
                          return_value(other.return_value),
                          extent(other.extent),
                          frames_(other.frames_),
                          symtable_(other.symtable_)
{
  add_ref(frames_);
}

VM::~VM(){
  release(frames_);
}


VM& VM::operator=(const VM& other){
  code = other.code;
  stack = other.stack;
  return_value = other.return_value;
  extent = other.extent,

  release(frames_);
  add_ref(other.frames_);
  frames_ = other.frames_;
  
  symtable_ = other.symtable_;

  return *this;
}


void VM::enter_frame(Env* e){
  frames_.push_back(e);
  add_ref(frame());
}  

void VM::leave_frame(){
  release(frame());
  frames_.pop_back();
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
    f << '[' << v.return_value.size() << "[ ";
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

  f << "--- [env stack] ---\n";
  for(auto i = v.frames_.rbegin(), e = v.frames_.rend(); i != e; ++i){
    f << **i;
  }

  f << "--- [symtable] ---\n";
  f << *v.symtable_;

  f << "\n\n";

  return f;
}
