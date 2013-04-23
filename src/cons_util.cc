#include "cons_util.hh"
#include "lisp_ptr.hh"
#include "cons.hh"
#include "zs_memory.hh"

#include <cassert>

void free_cons_list(Lisp_ptr p){
  // TODO: erase elements!
  
  auto i = begin(p);
  while(i){
    auto cell = i.base();
    ++i;
    zs_delete(cell.get<Cons*>());
  }

  if(!nullp(i.base())){
    // erase element
    ;
  }
}

// GrowList class
void GrowList::push(Lisp_ptr p){
  assert(head && next);
  assert(nullp(*next));

  auto newc = zs_new<Cons>(p, Cons::NIL);
  
  *next = {newc};
  next = &(newc->cdr_);
}

GrowList::~GrowList(){
  if(auto c = head.get<Cons*>()){
    free_cons_list(c);
  }
  // invalidate();
}


// ConsIter class
Lisp_ptr* ConsIter::operator->() const{
  if(!(*this)){
    throw zs_error("cons list error: dereferenced invalid ConsIter!\n");
  }
  return &(p_.get<Cons*>()->car_);
}

ConsIter& ConsIter::operator++(){
  if(p_.tag() != Ptr_tag::cons){
    throw zs_error("cons list error: forwarded ConsIter pointing the tail of a dotted list!\n");
  }

  if(auto c = p_.get<Cons*>()){
    p_ = c->cdr();
  }
  return *this;
}

ConsIter ConsIter::operator++(int){
  auto ret = *this;
  ++(*this);
  return ret;
}

ConsIter begin(Lisp_ptr p){
  if(p.tag() != Ptr_tag::cons){
    throw zs_error_arg1("cons func", "value is not a list", {p});
  }
  return ConsIter(p);
}

ConsIter end(Lisp_ptr p){
  // This is too slow, but needed for treating dotted-list correctly.
  // If you don't want to traverse, you can write without end(), like:
  //   for(auto i = begin(p); i; ++i){ ... }
  auto i = begin(p);
  while(i) ++i;
  return i;
}
