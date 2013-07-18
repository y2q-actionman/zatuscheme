#ifndef LISP_PTR_I_HH
#define LISP_PTR_I_HH

#ifndef LISP_PTR_HH
#error "Please include via parent file"
#endif

#include <cassert>
#include <type_traits>

// Lisp_ptr constructors
inline constexpr
Lisp_ptr::Lisp_ptr(bool b)
  : tag_(to_tag<bool>()), u_(b){}

inline constexpr
Lisp_ptr::Lisp_ptr(char c)
  : tag_(to_tag<char>()), u_(c){}

inline constexpr
Lisp_ptr::Lisp_ptr(int i)
  : tag_(to_tag<int>()), u_(i){}

template<typename T>
inline constexpr
Lisp_ptr::Lisp_ptr(T p)
  : tag_(to_tag<T>()), u_(p){
  static_assert(!std::is_fundamental<T>::value,
                "Lisp_ptr cannot accept the specified type.");
}

template<>
inline constexpr
Lisp_ptr::Lisp_ptr<Notation>(Notation n)
  : tag_(to_tag<Notation>()), u_(static_cast<int>(n)){}

// Lisp_ptr getters
template<>
inline
bool Lisp_ptr::get<bool>() const {
  assert(tag() == to_tag<bool>());
  return static_cast<bool>(u_.i_);
}

template<>
inline
char Lisp_ptr::get<char>() const {
  assert(tag() == to_tag<char>());
  return static_cast<char>(u_.i_);
}

template<>
inline
int Lisp_ptr::get<int>() const {
  assert(tag() == to_tag<int>());
  return static_cast<int>(u_.i_);
}

template<>
inline
VMop Lisp_ptr::get<VMop>() const {
  assert(tag() == to_tag<VMop>());
  return u_.f_;
}

template<>
inline
VMArgcount Lisp_ptr::get<VMArgcount>() const {
  assert(tag() == to_tag<VMArgcount>());
  return static_cast<VMArgcount>(u_.i_);
}

template<>
inline
Notation Lisp_ptr::get<Notation>() const {
  assert(tag() == to_tag<Notation>());
  return static_cast<Notation>(u_.i_);
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
inline
T Lisp_ptr::get() const {
  assert(tag() == to_tag<T>());
  return lisp_ptr_detail::lisp_ptr_cast<T>(u_.ptr_, u_.cptr_);
}

#endif // LISP_PTR_I_HH
