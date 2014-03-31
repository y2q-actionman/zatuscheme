#include "cons_util.hh"
#include "lisp_ptr.hh"
#include "zs_error.hh"

Lisp_ptr make_cons_list(std::initializer_list<Lisp_ptr> lis){
  return make_cons_list(begin(lis), end(lis));
}

// GrowList class
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
  next = &newc->cdr;
}

Lisp_ptr GrowList::extract_with_tail(Lisp_ptr p){
  *next = p;
  auto ret = head;
  invalidate();
  return ret;
}


// ConsIter class
Lisp_ptr ConsIter::operator*() const{
  check_nonnull_cons(p_);
  return p_.get<Cons*>()->car;
}

Lisp_ptr* ConsIter::operator->() const{
  check_nonnull_cons(p_);
  return &p_.get<Cons*>()->car;
}

ConsIter& ConsIter::operator++(){
  check_nonnull_cons(p_);
  p_ = p_.get<Cons*>()->cdr;
  return *this;
}

ConsIter ConsIter::operator++(int){
  auto ret = *this;
  ++(*this);
  return ret;
}

ConsIter::operator bool() const{
  return is_nonnull_cons(p_);
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
