#ifndef CONS_UTIL_HH
#define CONS_UTIL_HH

#include <initializer_list>
#include <iterator>
#include <array>
#include "cons.hh"

inline bool nullp(Lisp_ptr);

template<typename MainFun, typename LastFun>
auto do_list(Lisp_ptr p, MainFun&&, LastFun&& lf)
  -> decltype(lf(p));

template<typename MainFun, typename LastFun>
auto do_list_2(Lisp_ptr p, Lisp_ptr q, MainFun&&, LastFun&& lf)
  -> decltype(lf(p, q));

template<typename... Fun>
int bind_cons_list(Lisp_ptr, Fun&&...);

//these are defined in ".i.hh"
// template<typename Fun>
// auto bind_cons_list_loose(Lisp_ptr p, Fun fun);
// template<typename Fun>
// auto bind_cons_list_strict(Lisp_ptr p, Fun fun);

void free_cons_list(Lisp_ptr);

template<typename Iter>
Lisp_ptr make_cons_list(Iter, Iter);

Lisp_ptr make_cons_list(std::initializer_list<Lisp_ptr>);

Lisp_ptr push_cons_list(Lisp_ptr, Lisp_ptr);


class GrowList {
  Lisp_ptr head;
  Lisp_ptr* next;

private:
  void invalidate();

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
};


class ConsIter
  : public std::iterator<std::forward_iterator_tag, Lisp_ptr>
{
public:
  ConsIter() : p_(Cons::NIL){}
  explicit ConsIter(Lisp_ptr p) : p_(p){}
  ConsIter(const ConsIter&) = default;
  ConsIter(ConsIter&&) = default;

  ~ConsIter() = default;

  ConsIter& operator=(const ConsIter&) = default;
  ConsIter& operator=(ConsIter&&) = default;

  Lisp_ptr operator*() const
  { return (*this) ? p_.get<Cons*>()->car() : Lisp_ptr{}; }

  Lisp_ptr* operator->() const;

  ConsIter& operator++();
  ConsIter operator++(int);

  explicit operator bool() const
  { return (p_.get<Cons*>()); }


  Lisp_ptr base() const
  { return p_; }

private:
  Lisp_ptr p_;
};

bool operator==(const ConsIter&, const ConsIter&);
bool operator!=(const ConsIter&, const ConsIter&);

ConsIter begin(Lisp_ptr);
ConsIter end(Lisp_ptr);
  
template<int size>
std::array<Lisp_ptr, size> cons_list_to_array(Lisp_ptr);

#include "cons_util.i.hh"

#endif //CONS_UTIL_HH
