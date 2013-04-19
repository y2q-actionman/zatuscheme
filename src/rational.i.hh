#ifndef RATIONAL_I_HH
#define RATIONAL_I_HH

#ifndef RATIONAL_HH
#error "Please include via parent file"
#endif

#include <utility>

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

#endif // RATIONAL_I_HH
