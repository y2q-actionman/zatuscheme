#ifndef RATIONAL_I_HH
#define RATIONAL_I_HH

#ifndef RATIONAL_HH
#error "Please include via parent file"
#endif

#include <utility>

inline
bool operator!=(const Rational& r1, const Rational& r2){
  return !(r1 == r2);
}

inline
bool operator>(const Rational& r1, const Rational& r2){
  return (r2 < r1);
}

inline
bool operator<=(const Rational& r1, const Rational& r2){
  return !(r2 < r1);
}

inline
bool operator>=(const Rational& r1, const Rational& r2){
  return !(r1 < r2);
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
