#ifndef LISP_PTR_I_HH
#define LISP_PTR_I_HH

#ifndef LISP_PTR_HH
#error "Please include via parent file"
#endif

#include <cassert>
#include <type_traits>

// Lisp_ptr constructors
inline constexpr
Lisp_ptr::Lisp_ptr()
 : tag_(Ptr_tag::undefined), u_(0){}

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
  : tag_(to_tag<T>()),
    u_(static_cast<typename std::conditional<std::is_enum<T>::value,
                                             decltype(lisp_ptr_u::i_),
                                             T>
                   ::type>(p)){
  static_assert(!std::is_fundamental<T>::value,
                "Lisp_ptr cannot accept the specified type.");
}

// Lisp_ptr getters
template<>
inline constexpr
void* Lisp_ptr::get<void*>() const{
  return u_.ptr_;
}

namespace lisp_ptr_detail {

template<typename T>
inline constexpr
T lisp_ptr_cast(const Lisp_ptr& p,
                typename std::enable_if<std::is_integral<T>::value>::type* = nullptr){
  return static_cast<T>(p.u_.i_);
}

template<typename T>
inline constexpr
T lisp_ptr_cast(const Lisp_ptr& p,
                typename std::enable_if<std::is_class<T>::value>::type* = nullptr){
  static_assert(std::is_convertible<T, decltype(p.u_.i_)>::value,
                "inacceptable class type");
  return static_cast<T>(p.u_.i_);
}

template<typename T>
inline constexpr
T lisp_ptr_cast(const Lisp_ptr& p,
                typename std::enable_if<std::is_enum<T>::value>::type* = nullptr){
  // GCC 4.6 cannot accept below..
  // static_assert(std::is_convertible<typename std::underlying_type<T>::type,
  //                                   decltype(p.u_.i_)>::value,
  //               "inacceptable enum type");
  return static_cast<T>(p.u_.i_);
}

template<typename T>
inline constexpr
T lisp_ptr_cast(const Lisp_ptr& p,
                typename std::enable_if<std::is_pointer<T>::value>::type* = nullptr,
                typename std::enable_if<
                  std::is_function<typename std::remove_pointer<T>::type>::value
                  >::type* = nullptr){
  static_assert(std::is_same<T, decltype(p.u_.f_)>::value,
                "inacceptable function-pointer type");
  return static_cast<T>(p.u_.f_);
}

template<typename T>
inline constexpr
T lisp_ptr_cast(const Lisp_ptr& p,
                typename std::enable_if<std::is_pointer<T>::value>::type* = nullptr,
                typename std::enable_if<
                  !std::is_function<typename std::remove_pointer<T>::type>::value
                  >::type* = nullptr,
                typename std::enable_if<
                  !std::is_const<typename std::remove_pointer<T>::type>::value
                  >::type* = nullptr){
  return static_cast<T>(p.u_.ptr_);
}

template<typename T>
inline constexpr
T lisp_ptr_cast(const Lisp_ptr& p,
                typename std::enable_if<std::is_pointer<T>::value>::type* = nullptr,
                typename std::enable_if<
                  !std::is_function<typename std::remove_pointer<T>::type>::value
                  >::type* = nullptr,
                typename std::enable_if<
                  std::is_const<typename std::remove_pointer<T>::type>::value
                  >::type* = nullptr){
  return static_cast<T>(p.u_.cptr_);
}

}

template<typename T>
inline
T Lisp_ptr::get() const {
  static_assert(to_tag<T>() != Ptr_tag::undefined, "inacceptable type");
  assert(tag() == to_tag<T>());
  return lisp_ptr_detail::lisp_ptr_cast<T>(*this);
}

#endif // LISP_PTR_I_HH
