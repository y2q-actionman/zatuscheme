#ifndef CONS_HH
#define CONS_HH

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

  Lisp_ptr car() const;
  Lisp_ptr cdr() const;
  void rplaca(Lisp_ptr);
  void rplacd(Lisp_ptr);

private:
  Lisp_ptr car_;
  Lisp_ptr cdr_;
};

inline bool nullp(Lisp_ptr);

template<typename MainFun, typename LastFun>
auto do_list(Lisp_ptr p, MainFun&&, LastFun&& lf)
  -> decltype(lf(p));

template<typename... Fun>
int bind_cons_list(Lisp_ptr, Fun&&...);

void free_cons_list(Lisp_ptr);

#include "cons.i.hh"

#endif // CONS_HH
