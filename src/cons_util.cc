#include "cons_util.hh"
#include "lisp_ptr.hh"
#include "cons.hh"
#include "util.hh"

#include <cassert>

void free_cons_list(Lisp_ptr p){
  // TODO: erase elements!

  do_list(p,
          [](Cons* c) -> bool {
            // erase c->car()
            delete c;
            return true;
          },
          [](Lisp_ptr dot_cdr){
            if(!nullp(dot_cdr)){
              // erase element
              ;
            }
          });
}

void GrowList::push(Lisp_ptr p){
  assert(head && next);
  assert(*next == Cons::NIL);

  auto newc = new Cons(p, Cons::NIL);
  
  *next = {newc};
  next = &(newc->cdr_);
}


static zs_error make_cons_iter_error(Lisp_ptr p){
  return make_zs_error("cons list error: dot-list appeared for a proper-list procedure (%s appeared)\n",
                       stringify(p.tag()));
}


// GrowList class
GrowList::~GrowList(){
  if(auto c = head.get<Cons*>()){
    free_cons_list(c);
  }
  // invalidate();
}


// ConsIter class
Lisp_ptr* ConsIter::operator->() const{
  if(!(*this)){
    throw make_zs_error("cons list error: dereferenced invalid ConsIter!\n");
  }
  return &(p_.get<Cons*>()->car_);
}

ConsIter& ConsIter::operator++(){
  if(p_.tag() != Ptr_tag::cons){
    throw make_cons_iter_error(p_);
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
    throw make_cons_iter_error(p);
  }
  return ConsIter(p);
}

ConsIter end(Lisp_ptr p){
#ifndef NDEBUG
  if(p.tag() != Ptr_tag::cons){
    throw make_cons_iter_error(p);
  }
#else
  (void)p;
#endif

  return ConsIter();
}
