#ifndef CONS_HH
#define CONS_HH

#include <initializer_list>

#include "lisp_ptr.hh"

class Cons {
public:
  static constexpr Lisp_ptr NIL = Lisp_ptr{static_cast<Cons*>(nullptr)};

  constexpr Cons(Lisp_ptr = Lisp_ptr(), Lisp_ptr = Lisp_ptr());
  Cons(const Cons&) = default;
  Cons(Cons&&) = default;

  ~Cons() = default;

  Cons& operator=(const Cons&) = default;
  Cons& operator=(Cons&&) = default;

  inline Lisp_ptr car() const;
  inline Lisp_ptr cdr() const;
  inline void rplaca(Lisp_ptr);
  inline void rplacd(Lisp_ptr);

private:
  Lisp_ptr car_;
  Lisp_ptr cdr_;
};

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

#include "cons.i.hh"

#endif // CONS_HH
