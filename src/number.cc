#include <utility>
#include <cstdlib>
#include <cstring>

#include "number.hh"
#include "util.hh"

using namespace std;

// number class definitions.
template <>
Number::complex_type Number::coerce() const{
  switch(type_){
  case Type::complex:
    return z_;
  case Type::real:
    return complex_type{f_};
  case Type::integer:
    return complex_type{static_cast<real_type>(i_)};
  case Type::uninitialized:
  default:
    UNEXP_CONVERSION("complex");
  }
}

template <>
Number::real_type Number::coerce() const{
  switch(type_){
  case Type::real:
    return f_;
  case Type::integer:
    return static_cast<real_type>(i_);
  case Type::complex:
  case Type::uninitialized:
  default:
    UNEXP_CONVERSION("real");
  }
}

template <>
Number::integer_type Number::coerce() const{
  switch(type_){
  case Type::real:
    return static_cast<integer_type>(f_);
  case Type::integer:
    return i_;
  case Type::complex:
  case Type::uninitialized:
  default:
    UNEXP_CONVERSION("integer");
  }
}


// number parsers
namespace {

enum class Exactness{
  exact, inexact, unspecified
    };

struct PrefixValue {
  const int radix;
  const Exactness ex;

  constexpr PrefixValue() // error value
    : radix(0), ex(Exactness::unspecified){}

  constexpr PrefixValue(int r, Exactness e)
    : radix(r), ex(e){}

  explicit operator bool() const {
    return (radix != 0);
  }
};

PrefixValue parse_number_prefix(FILE* f){
  int r = 10;
  Exactness e = Exactness::unspecified;
  bool r_appeared = false, e_appeared = false;

  for(int loop = 0; loop < 2; ++loop){
    decltype(fgetc(f)) c = fgetc(f);

    if(c != '#'){
      ungetc(c, f);
      return {r, e};
    }

    switch(c = fgetc(f)){
    case 'i': case 'e':
      if(e_appeared){
        fprintf(zs::err, "reader error: duplicated number prefix appeared (%c)\n", c);
        return {};
      }
      e_appeared = true;
      e = (c == 'i') ? Exactness::inexact 
        : Exactness::exact;
      break;
    case 'b': case 'o': case 'd': case 'x':
      if(r_appeared){
        fprintf(zs::err, "reader error: duplicated number prefix appeared (%c)\n", c);
        return {};
      }
      r_appeared = true;
      r = (c == 'b') ? 2
        : (c == 'o') ? 8
        : (c == 'd') ? 10
        : 16;
      break;
    default:
      fprintf(zs::err, "reader error: unknown number prefix '%c' appeared!\n", c);
      return {};
    }
  }  
  
  return {r, e};
}


inline
bool is_number_char(int radix, char c){
  switch(radix){
  case 16:
    return isxdigit(c);

  case 10:
    return isdigit(c);

  case 8:
    switch(c){
    case '0': case '1':
    case '2': case '3': case '4':
    case '5': case '6': case '7':
      return true;
    default:
      return false;
    }

  case 2:
    switch(c){
    case '0': case '1':
      return true;
    default:
      return false;
    }
    
  default:
    UNEXP_DEFAULT();
  }
}

int eat_sharp(FILE* f, string& o){
  decltype(fgetc(f)) c;
  int sharps = 0;

  while((c = fgetc(f)) == '#'){
    o.push_back('0');
    ++sharps;
  }
  ungetc(c, f);

  return sharps;
}


struct ParserRet {
  const Number number;
  const Exactness ex;

  constexpr ParserRet() // error value
    : number(), ex(Exactness::unspecified){}

  constexpr ParserRet(const Number& n, Exactness e)
    : number(n), ex(e){}

  explicit operator bool() const {
    return static_cast<bool>(number);
  }
};

ParserRet parse_unsigned(int radix, FILE* f, string& s){
  decltype(fgetc(f)) c;

  while(is_number_char(radix, c = fgetc(f)))
    s.push_back(c);
  ungetc(c, f);

  if(s.empty()){
    return {}; // no digit char. starting with dot ?
  }
   
  Exactness e;

  if(eat_sharp(f, s) > 0){
    e = Exactness::inexact;
  }else{
    e = Exactness::exact;
  }

  errno = 0;
  auto l = strtol(s.c_str(), nullptr, radix);
  if(errno){
    auto eno = errno;
    char estr[128];
    strerror_r(eno, estr, sizeof(estr));

    fprintf(zs::err, "reader error: reading integer failed: %s\n", estr);
    return {};
  }

  return {Number{l}, e};
}

inline
bool check_decimal_suffix(char c){
  switch(c){
  case 'e': case 's': case 'f': case 'd': case 'l':
    return true;
  default:
    return false;
  }
}

ParserRet parse_decimal(FILE* f, string& s){
  decltype(fgetc(f)) c;
  bool dot_start = false;
  int sharps_before_dot = 0;

  if(s.empty()){
    if((c = fgetc(f)) == '.'){
      ungetc('.', f);
      dot_start = true;
    }else{
      fprintf(zs::err, "reader error: no chars found for floating point number.\n");
      return {};
    }
  }else{
    sharps_before_dot = eat_sharp(f, s);
  }

  if((c = fgetc(f)) != '.'){
    ungetc(c, f);
    goto end; // 1. no frac part
  }
  s.push_back('.');
    
  if(sharps_before_dot > 0){
    eat_sharp(f, s);
    goto end; // 4. only sharps after dot
  }

  {
    bool digits_after_dot = false;

    while(isdigit(c = fgetc(f))){
      digits_after_dot = true;
      s.push_back(c);
    }
    ungetc(c, f);

    if(dot_start && !digits_after_dot){
      fprintf(zs::err, "reader error: a number starting with dot should have digits after it.\n");
      return {};
    }

    eat_sharp(f, s);
  }
  
 end:
  if(check_decimal_suffix(c = fgetc(f))){
    s.push_back('e');

    switch(c = fgetc(f)){
    case '+': case '-':
      s.push_back(c); break;
    default:
      ungetc(c, f); break;
    }

    {
      bool exp_digits = false;

      while(isdigit(c = fgetc(f))){
        exp_digits = true;
        s.push_back(c);
      }
      ungetc(c, f);

      if(!exp_digits){
        fprintf(zs::err, "reader error: no number on exporational part\n");
        return {};
      }
    }
  }else{
    ungetc(c, f);
  }

  errno = 0;
  auto d = strtod(s.c_str(), nullptr);
  if(errno){
    auto eno = errno;
    char estr[128];
    strerror_r(eno, estr, sizeof(estr));

    fprintf(zs::err, "reader error: reading floating point number failed: %s\n", estr);
    return {};
  }

  return {Number{d}, Exactness::inexact};
}

ParserRet parse_real_number(int radix, FILE* f){
  decltype(fgetc(f)) c;
  int sign = 1;

  switch(c = fgetc(f)){
  case '+':
    sign = 1;
    break;
  case '-':
    sign = -1;
    break;
  default:
    sign = 1;
    ungetc(c, f);
    break;
  }

  string s;

  auto u1 = parse_unsigned(radix, f, s);
  c = fgetc(f);

  if((c == '.') || (u1 && check_decimal_suffix(c))){
    if(radix != 10){
      fprintf(zs::err, "reader error: non-decimal float is not supported. (radix %d)\n", radix);
      return {};
    }

    // decimal float
    ungetc(c, f);
    auto n = parse_decimal(f, s);

    if(!n){
      fprintf(zs::err, "reader error: failed at reading a decimal float\n");
      return {};
    }
      
    return {Number{n.number.coerce<double>() * sign},
        Exactness::inexact};
  }else if(!u1){
    fprintf(zs::err, "reader error: failed at reading a number's linteger part\n");
    return {};
  }else if(c == '/'){
    // rational
    string s2;
    auto u2 = parse_unsigned(radix, f, s2);
    if(!u2){
      fprintf(zs::err, "reader error: failed at reading a rational number's denominator\n");
      return {};
    }

    return {Number(sign * u1.number.coerce<double>() / u2.number.coerce<double>()),
        Exactness::inexact};
  }else{
    // integer?
    ungetc(c, f);
    // FIXME: inexact or super-big integer can be fall into float.
    return {Number(sign * u1.number.coerce<long>()), u1.ex};
  }
}

ParserRet parse_complex(int radix, FILE* f){
  const auto first_char = fgetc(f);
  ungetc(first_char, f);

  // has real part
  auto real = parse_real_number(radix, f);
  if(!real){
    fprintf(zs::err, "reader error: failed at reading a real number\n");
    return {};
  }

  switch(auto c = fgetc(f)){
  case '@': {// polar literal
    auto deg = parse_real_number(radix, f);
    if(!deg){
      fprintf(zs::err, "reader error: failed at reading a complex number's polar part.\n");
      return {};
    }
        
    return {Number{polar(real.number.coerce<double>(), deg.number.coerce<double>())},
        Exactness::inexact};
  }
  case '+': case '-': {
    const int sign = (c == '+') ? 1 : -1;

    if((c = fgetc(f)) == 'i'){
      return {Number{real.number.coerce<double>(), static_cast<double>(sign)},
          Exactness::inexact};
    }
    ungetc(c, f);
    
    auto imag = parse_real_number(radix, f);
    if(!imag || fgetc(f) != 'i'){
      fprintf(zs::err, "reader error: failed at reading a complex number's imaginary part.\n");
      return {};
    }

    return {Number{real.number.coerce<double>(), imag.number.coerce<double>() * sign},
        Exactness::inexact};
  }
  case 'i':
    if(first_char == '+' || first_char == '-'){
      return {Number{0, real.number.coerce<double>()},
          Exactness::inexact};
    }else{
      fprintf(zs::err, "reader error: failed at reading a complex number. ('i' appeared alone.)\n");
      return {};
    }
  default:
    ungetc(c, f);
    return real;
  }
}

} // namespace

Number parse_number(FILE* f){
  const auto prefix_info = parse_number_prefix(f);
  if(!prefix_info){
    fprintf(zs::err, "reader error: failed at reading a number's prefix\n");
    return {};
  }

  const auto r = parse_complex(prefix_info.radix, f);
  if(!r){
    fprintf(zs::err, "reader error: failed at reading a number\n");
    return {};
  }

  if(prefix_info.ex == Exactness::unspecified
     || prefix_info.ex == r.ex){
    return r.number;
  }else if(prefix_info.ex == Exactness::exact){
    return to_exact(r.number);
  }else{
    return to_inexact(r.number);
  }
}

bool eqv(const Number& n, const Number& m){
  if(n.type() != m.type()) return false;

  switch(n.type()){
  case Number::Type::uninitialized:
    return false;
  case Number::Type::complex:
    return n.get<Number::complex_type>() == m.get<Number::complex_type>();
  case Number::Type::real:
    return n.get<Number::real_type>() == m.get<Number::real_type>();
  case Number::Type::integer:
    return n.get<Number::integer_type>() == m.get<Number::integer_type>();
  default:
    UNEXP_DEFAULT();
  }
}

void print(FILE* f, const Number& n){
  switch(n.type()){
  case Number::Type::uninitialized:
    fprintf(f, "(uninitialied number)");
    break;
  case Number::Type::complex: {
    auto&& z = n.get<Number::complex_type>();
    fprintf(f, "%g+%gi", z.real(), z.imag());
  }
    break;
  case Number::Type::real:
    fprintf(f, "%g", n.get<Number::real_type>());
    break;
  case Number::Type::integer:
    fprintf(f, "%ld", n.get<Number::integer_type>());
    break;
  default:
    UNEXP_DEFAULT();
  }
}

Number to_exact(const Number& n){
  switch(n.type()){
  case Number::Type::complex:
    fprintf(zs::err, "number error: conversion from complex to exact number is not supprted.\n");
    return {}; // not supported
  case Number::Type::real:
    return Number{n.coerce<Number::integer_type>()};
  case Number::Type::integer:
    return n;
  case Number::Type::uninitialized:
  default:
    return {};
  }
}

Number to_inexact(const Number& n){
  switch(n.type()){
  case Number::Type::complex:
    return n;
  case Number::Type::real:
    return n;
  case Number::Type::integer:
    return Number{n.coerce<Number::real_type>()};
  case Number::Type::uninitialized:
  default:
    return {};
  }
}

const char* stringify(Number::Type t){
  switch(t){
  case Number::Type::uninitialized:
    return "uninitialized";
  case Number::Type::complex:
    return "complex";
  case Number::Type::real:
    return "real";
  case Number::Type::integer:
    return "integer";
  default:
    return "(unknown number type)";
  }
}
