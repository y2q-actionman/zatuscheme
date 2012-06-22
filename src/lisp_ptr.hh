#ifndef LISP_PTR_HH
#define LISP_PTR_HH

#include <string>
#include <vector>
#include <cstdio>
#include <unordered_map>

enum class Ptr_tag {
  undefined = -1,
  boolean = 0,
    character,
    cons,
    symbol,
    i_procedure,
    n_procedure,
    number,
    string,
    vector,
    port,
    env,
    vm_op
    };
    

class Lisp_ptr {
public:
  constexpr Lisp_ptr() : tag_(Ptr_tag::undefined), u_(){}
  template<typename T>
  explicit constexpr Lisp_ptr(T);
  Lisp_ptr(const Lisp_ptr&) = default;
  Lisp_ptr(Lisp_ptr&&) = default;

  ~Lisp_ptr() = default;

  Lisp_ptr& operator=(const Lisp_ptr&) = default;
  Lisp_ptr& operator=(Lisp_ptr&&) = default;

  Ptr_tag tag() const
  { return tag_; }

  template<typename T>
  T get() const;

  explicit constexpr operator bool() const
  { return (tag_ != Ptr_tag::undefined); }

private:
  union lisp_ptr_u{
    constexpr lisp_ptr_u(){}
    constexpr lisp_ptr_u(void* p) : ptr_(p){}
    constexpr lisp_ptr_u(const void* p) : cptr_(p){}
    constexpr lisp_ptr_u(bool b) : b_(b){}
    constexpr lisp_ptr_u(char c) : c_(c){}
    constexpr lisp_ptr_u(int i) : i_(i){}
    constexpr lisp_ptr_u(void(*f)()) : f_(f){}

    void* ptr_;
    const void* cptr_;
    bool b_;
    char c_;
    int i_;
    void (*f_)(void);
  };

  Ptr_tag tag_;
  lisp_ptr_u u_;
};


// typedefs & declarations
class Cons;
class Symbol;
namespace Procedure{
  class IProcedure;
  class NProcedure;
}
class Number;
typedef std::string String;
typedef std::vector<Lisp_ptr> Vector;
typedef FILE Port;
typedef std::unordered_map<Symbol*, Lisp_ptr> Env;
typedef void(*VM_op)();


const char* stringify(Ptr_tag);

void describe(FILE*, Lisp_ptr);

#include "lisp_ptr.i.hh"

#endif // LISP_PTR_HH
