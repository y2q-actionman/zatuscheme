#include <cassert>

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

void print(FILE* f, const VM& v){
  fputs("--- [code] ---\n", f);
  for(auto i = v.code.rbegin(), e = v.code.rend(); i != e; ++i){
    // print(f, *i);
    fputc('\n', f);
  }

  fputs("--- [stack] ---\n", f);
  for(auto i = v.stack.rbegin(), e = v.stack.rend(); i != e; ++i){
    // print(f, *i);
    fputc('\n', f);
  }

  fputs("--- [return value] ---\n", f);
  for(auto i = v.return_value.begin(), e = v.return_value.end(); i != e; ++i){
    fprintf(f, "[%zd] ", v.return_value.size());
    // print(f, *i);
    if(next(i) != e) fputs(", ", f);
  }

  if(!v.extent.empty()){
    fputs("--- [extent] ---\n", f);
    for(auto i = v.extent.begin(), e = v.extent.end(); i != e; ++i){
      // print(f, i->thunk);
      fputs(": ", f);

      // print(f, i->before);
      fputs(", ", f);
      // print(f, i->after);
      fputs("\n", f);
    }
  }

  // fputs("--- [env stack] ---\n", f);
  // for(auto i = v.frames_.rbegin(), e = v.frames_.rend(); i != e; ++i){
  //   print(f, **i);
  // }

  // fputs("--- [symtable] ---\n", f);
  // print(f, *v.symtable_);

  fputs("\n\n", f);
}
