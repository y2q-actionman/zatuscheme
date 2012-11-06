#ifndef CONS_UTIL_HH
#define CONS_UTIL_HH

#include <initializer_list>
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

void free_cons_list(Lisp_ptr);

template<typename Iter>
Lisp_ptr make_cons_list(Iter, Iter);

Lisp_ptr make_cons_list(std::initializer_list<Lisp_ptr>);

Lisp_ptr push_cons_list(Lisp_ptr, Lisp_ptr);

#include "cons_util.i.hh"

#endif //CONS_UTIL_HH
