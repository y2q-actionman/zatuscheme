#include <unordered_map>
#include <algorithm>
#include "zs_memory.hh"
#include "vm.hh"
#include "cons.hh"
#include "delay.hh"
#include "env.hh"
#include "procedure.hh"
#include "s_closure.hh"
#include "symbol.hh"
#include "s_rules.hh"
#include "zs_error.hh"
#include "rational.hh"

#include <iostream>
#include "printer.hh"

// Assumes 'delete' is not called frequently.

using namespace std;

namespace {

enum MarkBit {
  unmarked = 0,
  marked = 1,
  fixed = 2
};

struct MarkObj {
  Ptr_tag tag;
  MarkBit mark;
};

static unordered_map<void*, MarkObj> arena;

}

void* zs_m_in(void* p, Ptr_tag tag){
  arena[p] = {tag, MarkBit::unmarked};
  return p;
}

void zs_m_out(void* p){
  auto i = arena.find(p);
  if(i != end(arena)){
    arena.erase(i);
  }
}


// GC

namespace {

bool gc_is_marked_ptr(void* p){
  auto i = arena.find(p);
  return (i == end(arena)) || i->second.mark;
}

void gc_mark_ptr(void* p){
  auto i = arena.find(p);

  if(i != end(arena) && !i->second.mark){
    i->second.mark = MarkBit::marked;
  }
}
  
void gc_mark_lp(Lisp_ptr p);
    
void gc_mark(Cons* c){
  if(gc_is_marked_ptr(c)) return;
  gc_mark_ptr(c);

  if(!c) return;
  gc_mark_lp(c->car());
  gc_mark_lp(c->cdr());
}

void gc_mark(IProcedure* iproc){
  if(gc_is_marked_ptr(iproc)) return;
  gc_mark_ptr(iproc);

  gc_mark_lp(iproc->arg_list());
  gc_mark_lp(iproc->get());
  gc_mark(iproc->closure());
  gc_mark_lp(iproc->name());
}

void gc_mark(const VM& v){
  for(auto i : v.code){
    gc_mark_lp(i);
  }

  for(auto i : v.stack){
    gc_mark_lp(i);
  }

  for(auto i : v.return_value){
    gc_mark_lp(i);
  }

  for(auto i : v.extent){
    gc_mark_lp(i.before);
    gc_mark_lp(i.thunk);
    gc_mark_lp(i.after);
  }
  
  gc_mark(v.frame());
}

void gc_mark(Continuation* c){
  if(gc_is_marked_ptr(c)) return;
  gc_mark_ptr(c);

  gc_mark(c->get());
}

void gc_mark(Vector* v){
  if(gc_is_marked_ptr(v)) return;
  gc_mark_ptr(v);

  for(auto i : *v){
    gc_mark_lp(i);
  }
}

void gc_mark(Delay* d){
  if(gc_is_marked_ptr(d)) return;
  gc_mark_ptr(d);

  gc_mark_lp(d->get());
  gc_mark(d->env());
}

void gc_mark(SyntacticClosure* sc){
  if(gc_is_marked_ptr(sc)) return;
  gc_mark_ptr(sc);

  gc_mark(sc->env());
  gc_mark(sc->free_names());
  gc_mark_lp(sc->expr());
}

void gc_mark(SyntaxRules* sr){
  if(gc_is_marked_ptr(sr)) return;
  gc_mark_ptr(sr);

  gc_mark(sr->env());
  gc_mark_lp(sr->literals());
  gc_mark_lp(sr->rules());
}

} // namespace

void gc_mark(Env* e){
  if(gc_is_marked_ptr(e)) return;
  gc_mark_ptr(e);

  if(!e) return;
  for(auto i : e->map_){
    gc_mark_lp(i.first);
    gc_mark_lp(i.second);
  }
  gc_mark(e->next_);
}

namespace {

void gc_mark_lp(Lisp_ptr p){
  auto tag = p.tag();

  if(gc_is_marked_ptr(p.get<void*>()))
    return;

  switch(tag){
    // included in Lisp_ptr
  case Ptr_tag::undefined:
  case Ptr_tag::boolean:
  case Ptr_tag::character:
  case Ptr_tag::integer:
  case Ptr_tag::vm_argcount:
    break;

    // no dynamic allocation
  case Ptr_tag::n_procedure:
  case Ptr_tag::vm_op:
    break;

    // not container
  case Ptr_tag::symbol:
  case Ptr_tag::rational:
  case Ptr_tag::real:
  case Ptr_tag::complex:
  case Ptr_tag::string:
  case Ptr_tag::input_port:
  case Ptr_tag::output_port:
    gc_mark_ptr(p.get<void*>());
    break;

    // container
  case Ptr_tag::cons:
    gc_mark(p.get<Cons*>());
    break;
  case Ptr_tag::i_procedure:
    gc_mark(p.get<IProcedure*>());
    break;
  case Ptr_tag::continuation:
    gc_mark(p.get<Continuation*>());
    break;
  case Ptr_tag::vector:
    gc_mark(p.get<Vector*>());
    break;
  case Ptr_tag::env:
    gc_mark(p.get<Env*>());
    break;
  case Ptr_tag::delay:
    gc_mark(p.get<Delay*>());
    break;
  case Ptr_tag::syntactic_closure:
    gc_mark(p.get<SyntacticClosure*>());
    break;
  case Ptr_tag::syntax_rules:
    gc_mark(p.get<SyntaxRules*>());
    break;

  default:
    break;
  }
}

template<Ptr_tag tag>
void gc_tagged_delete(void* p){
  typedef typename to_type<Ptr_tag, tag>::type TargetT;
  delete static_cast<TargetT>(p);
}

void gc_sweep(){
  auto i = begin(arena), e = end(arena);
  while(i != e){
    auto ii = next(i);
    switch(i->second.mark){
    case MarkBit::unmarked:
      switch(i->second.tag){
      case Ptr_tag::undefined:
        break;
      case Ptr_tag::boolean:
        break;
      case Ptr_tag::character:
        break;
      case Ptr_tag::cons:
        gc_tagged_delete<Ptr_tag::cons>(i->first);
        break;
      case Ptr_tag::symbol: 
        gc_tagged_delete<Ptr_tag::symbol>(i->first);
        break;
      case Ptr_tag::i_procedure: 
        gc_tagged_delete<Ptr_tag::i_procedure>(i->first);
        break;
      case Ptr_tag::n_procedure:
        break;
      case Ptr_tag::continuation: 
        gc_tagged_delete<Ptr_tag::continuation>(i->first);
        break;
      case Ptr_tag::integer: 
        break;
      case Ptr_tag::rational: 
        gc_tagged_delete<Ptr_tag::rational>(i->first);
        break;
      case Ptr_tag::real: 
        gc_tagged_delete<Ptr_tag::real>(i->first);
        break;
      case Ptr_tag::complex: 
        gc_tagged_delete<Ptr_tag::complex>(i->first);
        break;
      case Ptr_tag::string: 
        gc_tagged_delete<Ptr_tag::string>(i->first);
        break;
      case Ptr_tag::vector: 
        gc_tagged_delete<Ptr_tag::vector>(i->first);
        break;
      case Ptr_tag::input_port: 
        gc_tagged_delete<Ptr_tag::input_port>(i->first);
        break;
      case Ptr_tag::output_port: 
        gc_tagged_delete<Ptr_tag::output_port>(i->first);
        break;
      case Ptr_tag::env: 
        gc_tagged_delete<Ptr_tag::env>(i->first);
        break;
      case Ptr_tag::delay: 
        gc_tagged_delete<Ptr_tag::delay>(i->first);
        break;
      case Ptr_tag::syntactic_closure: 
        gc_tagged_delete<Ptr_tag::syntactic_closure>(i->first);
        break;
      case Ptr_tag::syntax_rules: 
        gc_tagged_delete<Ptr_tag::syntax_rules>(i->first);
        break;
      case Ptr_tag::vm_op: 
        break;
      case Ptr_tag::vm_argcount:
        break;
      default:
        break;
      }
      arena.erase(i);
      break;
    case MarkBit::marked:
      i->second.mark = MarkBit::unmarked;
      break;
    case MarkBit::fixed:
      break;
    default:
      break;
    }
    i = ii;
  }
}

} // namespace

void gc(){
  gc_mark(vm);
  gc_sweep();
}
