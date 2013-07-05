#ifndef LISP_PTR_I_HH
#define LISP_PTR_I_HH

#ifndef LISP_PTR_HH
#error "Please include via parent file"
#endif

#include <type_traits>

// ptr class definitions

inline constexpr
Lisp_ptr::Lisp_ptr(bool b)
  : tag_(to_tag<bool>()), u_(b){}

inline constexpr
Lisp_ptr::Lisp_ptr(char c)
  : tag_(to_tag<char>()), u_(c){}

template<typename T>
inline constexpr
Lisp_ptr::Lisp_ptr(T p)
  : tag_(to_tag<T>()), u_(p){
  static_assert(!std::is_fundamental<T>::value,
                "Lisp_ptr cannot accept the specified type.");
}

inline constexpr
Lisp_ptr::Lisp_ptr(Ptr_tag p, int i)
  : tag_(p), u_(i){}

inline constexpr
Lisp_ptr::Lisp_ptr(Notation n)
  : tag_(to_tag<Notation>()), u_(static_cast<int>(n)){}


template<>
inline constexpr
bool Lisp_ptr::get<bool>() const {
  return (tag() == to_tag<bool>())
    ? u_.b_
    : operator bool(); // anything is #t, except #f and null
}

template<>
inline constexpr
char Lisp_ptr::get<char>() const {
  return (tag() == to_tag<char>())
    ? u_.c_ : '\0';
}

template<>
inline constexpr
VMop Lisp_ptr::get<VMop>() const {
  return (tag() == to_tag<VMop>())
     ? u_.f_ : nullptr;
}

template<>
inline constexpr
VMArgcount Lisp_ptr::get<VMArgcount>() const {
  return static_cast<VMArgcount>
    ((tag() == to_tag<VMArgcount>()) ? u_.i_ : 0);
}

template<>
inline constexpr
Notation Lisp_ptr::get<Notation>() const {
  return static_cast<Notation>
    ((tag() == to_tag<Notation>()) ? u_.i_ : 0);
}

template<>
inline constexpr
int Lisp_ptr::get<int>() const {
  return (tag() == Ptr_tag::integer)
    ? u_.i_ : 0;
}

template<>
inline constexpr
void* Lisp_ptr::get<void*>() const{
  return u_.ptr_;
}

namespace lisp_ptr_detail {

template<typename T>
inline constexpr
T lisp_ptr_cast(void* p, const void*,
                typename std::enable_if<
                  !std::is_const<typename std::remove_pointer<T>::type>::value
                  >::type* = nullptr){
  return static_cast<T>(p);
}

template<typename T>
inline constexpr
T lisp_ptr_cast(void*, const void* cp,
                typename std::enable_if<
                  std::is_const<typename std::remove_pointer<T>::type>::value
                  >::type* = nullptr){
  return static_cast<T>(cp);
}

}

template<typename T>
inline constexpr
T Lisp_ptr::get() const {
  return (tag() == to_tag<T>())
    ? lisp_ptr_detail::lisp_ptr_cast<T>(u_.ptr_, u_.cptr_)
    : nullptr;
}

#endif // LISP_PTR_I_HH
