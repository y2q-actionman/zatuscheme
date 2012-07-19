#ifndef UTIL_I_HH
#define UTIL_I_HH

#ifndef UTIL_HH
#error "Please include via parent file"
#endif

#include <type_traits>

namespace zs {
  namespace call_traits_detail {
    template<typename T, bool b = std::is_scalar<T>::value>
    struct call_traits_i;

    template<typename T>
    struct call_traits_i<T, true>{
      typedef T l_type;
      typedef T r_type;
    };

    template<typename T>
    struct call_traits_i<T, false>{
      typedef typename std::add_lvalue_reference<
        typename std::add_const<T>::type
        >::type l_type;
      typedef typename std::add_rvalue_reference<T>::type r_type;
    };
  }

  template<typename T>
  struct call_traits{
    typedef typename call_traits_detail::call_traits_i<T>::l_type type;
  };

  template<typename T>
  struct call_traits_r{
    typedef typename call_traits_detail::call_traits_i<T>::r_type type;
  };
}

#endif // UTIL_I_HH
