#ifndef LISP_PTR_HH
#define LISP_PTR_HH

#include <cstdint>

/* tag bit map

 __00: immediate
       bit 0~1            : 0
       bit 2              : boolean
       bit ~CHAR_BIT-1    : keyword
       bit ~(CHAR_BIT * 2): char
       bit ~rest          : (unused)

 __01: cons
 __10: symbol
 __11: long ptr

 */

enum class Ptr_tag {
  unknown = -1,
  /* in Lisp_ptr */
    immediate = 0x0,
    cons = 0x1,
    symbol = 0x2,
    long_ptr = 0x3,
  /* in Long_ptr */
    function,
    number,
    string,
    vector,
    port
    };
    
class Lisp_ptr{
public:
  static constexpr unsigned tag_bit_mask = 0x3u;
  static constexpr unsigned embed_boolean_bit = 0x4u;
  static constexpr unsigned embed_keyword_start_bit = 3;
  static constexpr unsigned embed_keyword_mask = 0x1fu;

  Lisp_ptr() : base_(0){}
  template<typename T>
  explicit Lisp_ptr(T);
  Lisp_ptr(const Lisp_ptr&) = default;
  Lisp_ptr(Lisp_ptr&&) = default;

  ~Lisp_ptr() = default;

  Lisp_ptr& operator=(const Lisp_ptr&) = default;
  Lisp_ptr& operator=(Lisp_ptr&&) = default;

  Ptr_tag tag() const;

  template<typename T>
  T get() const;

private:
  union {
    void* ptr_;
    uintptr_t base_;
  };
};


class Long_ptr {
public:
  Long_ptr() = delete;
  template<typename T>
  explicit Long_ptr(T);
  Long_ptr(const Long_ptr&) = default;
  Long_ptr(Long_ptr&&) = default;

  ~Long_ptr() = default;

  Long_ptr& operator=(const Long_ptr&) = default;
  Long_ptr& operator=(Long_ptr&&) = default;

  Ptr_tag tag() const
  { return tag_; }

  template<typename T>
  T get() const;

private:
  Ptr_tag tag_;
  void* ptr_;
};

#include "lisp_ptr.i.hh"

#endif // LISP_PTR_HH
