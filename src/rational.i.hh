#ifndef RATIONAL_I_HH
#define RATIONAL_I_HH

#ifndef RATIONAL_HH
#error "Please include via parent file"
#endif

#include "zs_error.hh"
#include "zs_memory.hh"

inline
Rational::Rational(int n)
  : overflow_(false){
  ratio_ = {n, 1};
}

inline
Rational::Rational(long long n, long long d)
  : overflow_(false){
  normalized_reset(n, d);
}

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
Rational::operator int() const{
  assert(is_convertible<int>());
  return numerator();
}

inline
bool Rational::operator==(const Rational& other) const{
  if(overflow_) return false;

  // assumes rationals are normalized.
  return (numerator() == other.numerator()) && (denominator() == other.denominator());
}

inline
bool Rational::operator<(const Rational& other) const{
  if(overflow_) return false;

  return (numerator() * other.denominator()) < (other.numerator() * denominator());
}

inline
Rational& Rational::operator+=(const Rational& other){
  if(overflow_) return *this;

  auto n = (long long)numerator() * other.denominator()
    + (long long)other.numerator() * denominator();
  auto d = (long long)denominator() * other.denominator();

  return normalized_reset(n, d);
}

inline
Rational& Rational::operator-=(const Rational& other){
  if(overflow_) return *this;

  auto n = (long long)numerator() * other.denominator()
    - (long long)other.numerator() * denominator();
  auto d = (long long)denominator() * other.denominator();

  return normalized_reset(n, d);
}

inline
Rational& Rational::operator*=(const Rational& other){
  if(overflow_) return *this;

  auto n = (long long)numerator() * other.numerator();
  auto d = (long long)denominator() * other.denominator();

  return normalized_reset(n, d);
}

inline
Rational& Rational::operator/=(const Rational& other){
  if(overflow_) return *this;

  auto n = (long long)numerator() * other.denominator();
  auto d = (long long)denominator() * other.numerator();

  return normalized_reset(n, d);
}

inline
Rational& Rational::negate(){
  if(overflow_) return *this;
  return normalized_reset(-(long long)numerator(), (long long)denominator());
}

inline
Rational& Rational::inverse(){
  if(overflow_) return *this;
  return normalized_reset((long long)denominator(), (long long)numerator());
}

// utilities
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

inline
Lisp_ptr wrap_number(int i){
  return Lisp_ptr{i};
}

inline
Lisp_ptr wrap_number(bool b){
  return Lisp_ptr{b};
}

inline
Lisp_ptr wrap_number(Lisp_ptr p){
  return p;
}

#endif // RATIONAL_I_HH
