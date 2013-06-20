#ifndef RATIONAL_I_HH
#define RATIONAL_I_HH

#ifndef RATIONAL_HH
#error "Please include via parent file"
#endif

#include <utility>

template <typename T>
bool Rational::is_convertible() const = delete;

template <>
inline
bool Rational::is_convertible<int>() const{
  return (!overflow_) && (denominator() == 1);
}

template <>
inline
bool Rational::is_convertible<Rational>() const{
  return (!overflow_);
}

template <>
inline
bool Rational::is_convertible<double>() const{
  return true;
}

inline
bool Rational::operator!=(const Rational& other) const{
  return !(*this == other);
}

inline
bool Rational::operator>(const Rational& other) const{
  return (other < *this);
}

inline
bool Rational::operator<=(const Rational& other) const{
  return !(other < *this);
}

inline
bool Rational::operator>=(const Rational& other) const{
  return !(*this < other);
}

template<typename T>
T gcd(T m, T n){
  if(m < 0) m = -m;
  if(n < 0) n = -n;

  if(m < n)
    std::swap(m, n);

  while(n > 0){
    auto mod = m % n;
    m = n;
    n = mod;
  }

  return m;
}


template<> int coerce(Lisp_ptr);
template<> Rational coerce(Lisp_ptr);
template<> double coerce(Lisp_ptr);
template<> Complex coerce(Lisp_ptr);

#endif // RATIONAL_I_HH
