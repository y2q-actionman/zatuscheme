#include <cassert>

#include "cons_util.hh"
#include "lisp_ptr.hh"
#include "zs_error.hh"

Lisp_ptr make_cons_list(std::initializer_list<Lisp_ptr> lis){
  return make_cons_list(begin(lis), end(lis));
}

// GrowList class
GrowList::GrowList()
  : head(Cons::NIL), next(&head)
{}

GrowList::~GrowList(){
  if(head){
    auto i = begin(head);
    while(i){
      auto cell = i.base();
      ++i;
      if(cell.tag() == Ptr_tag::cons)
        zs_delete(cell.get<Cons*>());
    }
  }
  // invalidate();
}

void GrowList::push(Lisp_ptr p){
  assert(head && next);
  assert(nullp(*next));

  auto newc = zs_new<Cons>(p, Cons::NIL);
  
  *next = {newc};
  next = &(newc->cdr_);
}

Lisp_ptr GrowList::extract_with_tail(Lisp_ptr p){
  *next = p;
  auto ret = head;
  invalidate();
  return ret;
}

void GrowList::invalidate(){
  assert(head && next);
  head = {};
  next = nullptr;
}  


// ConsIter class
Lisp_ptr ConsIter::operator*() const{
  if(!*this){
    throw_zs_error(p_, "cons list error: dereferenced invalid ConsIter!\n");
  }
  return car(p_.get<Cons*>());
}

Lisp_ptr* ConsIter::operator->() const{
  if(!*this){
    throw_zs_error(p_, "cons list error: dereferenced invalid ConsIter!\n");
  }
  return &(p_.get<Cons*>()->car_);
}

ConsIter& ConsIter::operator++(){
  if(!*this){
    throw_zs_error(p_, "cons list error: advanced invalid ConsIter!\n");
  }
  p_ = cdr(p_.get<Cons*>());
  return *this;
}

ConsIter ConsIter::operator++(int){
  auto ret = *this;
  ++(*this);
  return ret;
}

ConsIter::operator bool() const{
  return (p_.tag() == Ptr_tag::cons) && !nullp(p_);
}

ConsIter begin(Lisp_ptr p){
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

bool operator==(const ConsIter& i1, const ConsIter& i2){
  return eq_internal(i1.base(), i2.base());
}

bool operator!=(const ConsIter& i1, const ConsIter& i2){
  return !eq_internal(i1.base(), i2.base());
}
