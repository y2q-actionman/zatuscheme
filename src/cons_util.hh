#ifndef CONS_UTIL_HH
#define CONS_UTIL_HH

#include <initializer_list>
#include <iterator>
#include "cons.hh"

bool nullp(Lisp_ptr);
bool is_nonnull_cons(Lisp_ptr);

template<typename Iter>
Lisp_ptr make_cons_list(Iter, Iter);

Lisp_ptr make_cons_list(std::initializer_list<Lisp_ptr>);


template<unsigned>
Lisp_ptr nth_cons_list(Lisp_ptr);

template<unsigned>
Lisp_ptr nthcdr_cons_list(Lisp_ptr);


class GrowList {
public:
  GrowList();
  // GrowList(Cons*); // starting with an existing list.
  GrowList(const GrowList&) = delete;
  GrowList(GrowList&&) = delete;
  
  ~GrowList();

  GrowList& operator=(const GrowList&) = delete;
  GrowList& operator=(GrowList&&) = delete;

  void push(Lisp_ptr);
  Lisp_ptr extract();
  Lisp_ptr extract_with_tail(Lisp_ptr);

private:
  Lisp_ptr head;
  Lisp_ptr* next;

  void invalidate();
};


class ConsIter
  : public std::iterator<std::forward_iterator_tag, Lisp_ptr>
{
public:
  ConsIter() : p_(){}
  explicit ConsIter(Lisp_ptr p) : p_(p){}
  ConsIter(const ConsIter&) = default;
  ConsIter(ConsIter&&) = default;

  ~ConsIter() = default;

  ConsIter& operator=(const ConsIter&) = default;
  ConsIter& operator=(ConsIter&&) = default;

  Lisp_ptr operator*() const;
  Lisp_ptr* operator->() const;

  ConsIter& operator++();
  ConsIter operator++(int);

  explicit operator bool() const;

  Lisp_ptr base() const
  { return p_; }

private:
  Lisp_ptr p_;
};

bool operator==(const ConsIter&, const ConsIter&);
bool operator!=(const ConsIter&, const ConsIter&);

ConsIter begin(Lisp_ptr);
ConsIter end(Lisp_ptr);
  
#include "cons_util.i.hh"

#endif //CONS_UTIL_HH
