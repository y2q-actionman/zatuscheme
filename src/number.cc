#include <utility>
#include <cstdlib>
#include <cstring>
#include <iterator>
#include <istream>
#include <ostream>
#include <stdexcept>

#include "number.hh"
#include "zs_error.hh"

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
  int radix;
  Exactness ex;
};

PrefixValue parse_number_prefix(istream& f){
  int r = 10;
  Exactness e = Exactness::unspecified;
  bool r_appeared = false, e_appeared = false;

  for(int loop = 0; loop < 2; ++loop){
    auto c = f.get();

    if(c != '#'){
      f.unget();
      return {r, e};
    }

    switch(c = f.get()){
    case 'i': case 'e':
      if(e_appeared){
        throw zs_error(printf_string("reader error: duplicated number prefix appeared (%c)\n", c));
      }
      e_appeared = true;
      e = (c == 'i') ? Exactness::inexact 
        : Exactness::exact;
      break;
    case 'b': case 'o': case 'd': case 'x':
      if(r_appeared){
        throw zs_error(printf_string("reader error: duplicated number prefix appeared (%c)\n", c));
      }
      r_appeared = true;
      r = (c == 'b') ? 2
        : (c == 'o') ? 8
        : (c == 'x') ? 16
        : 10;
      break;
    default:
      throw zs_error(printf_string("reader error: unknown number prefix '%c' appeared!\n", c));
    }
  }  
  
  return {r, e};
}


inline
bool is_number_char(int radix, int c){
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

int eat_sharp(istream& f, string& o){
  decltype(f.get()) c;
  int sharps = 0;

  while((c = f.get()) == '#'){
    o.push_back('0');
    ++sharps;
  }
  f.unget();

  return sharps;
}


struct ParserRet {
  const Number number;
  const Exactness ex;
};

pair<string, Exactness> collect_integer_digits(int radix, istream& f){
  decltype(f.get()) c;
  string s;

  while(is_number_char(radix, c = f.get()))
    s.push_back(c);
  f.unget();

  Exactness e;

  if(eat_sharp(f, s) > 0){
    e = Exactness::inexact;
  }else{
    e = Exactness::exact;
  }

  return {s, e};
}
  

Number::integer_type zs_stol(int radix, const string& s){
  try{
    return std::stol(s, nullptr, radix);
  }catch(const std::logic_error& err){
    throw zs_error(printf_string("reader error: reading integer failed: %s\n", err.what()));
  }
}

inline
bool check_decimal_suffix(int c){
  switch(c){
  case 'e': case 's': case 'f': case 'd': case 'l':
    return true;
  default:
    return false;
  }
}

Number::real_type zs_stof(istream& f, string s){
  decltype(f.get()) c;

  // treating dot char
  bool dot_start = false;
  int sharps_before_dot = 0;

  if(s.empty()){
    if(f.peek() == '.'){
      dot_start = true;
    }else{
      throw zs_error("reader error: no chars found for floating point number.\n");
    }
  }else{
    sharps_before_dot = eat_sharp(f, s);
  }

  if(f.get() != '.'){
    f.unget();
    goto end; // 1. no frac part
  }
  s.push_back('.');
    
  if(sharps_before_dot > 0){
    eat_sharp(f, s);
    goto end; // 4. only sharps after dot
  }

  // treating fractional part
  {
    bool digits_after_dot = false;

    while(isdigit(c = f.get())){
      digits_after_dot = true;
      s.push_back(c);
    }
    f.unget();

    if(dot_start && !digits_after_dot){
      throw zs_error("reader error: a number starting with dot should have digits after it.\n");
    }

    eat_sharp(f, s);
  }
  
 end:
  // treating exporational part
  if(check_decimal_suffix(f.peek())){
    f.ignore(1);
    s.push_back('e');

    switch(f.peek()){
    case '+': case '-':
      s.push_back(f.get());
      break;
    default:
      break;
    }

    {
      bool exp_digits = false;

      while(isdigit(c = f.get())){
        exp_digits = true;
        s.push_back(c);
      }
      f.unget();

      if(!exp_digits){
        throw zs_error("reader error: no number on exporational part\n");
      }
    }
  }

  try{
    return std::stod(s, nullptr);
  }catch(const std::logic_error& err){
    throw zs_error(printf_string("reader error: reading floating point number failed: %s\n", err.what()));
  }
}

ParserRet parse_real_number(int radix, istream& f){
  int sign = 1;

  switch(f.peek()){
  case '+':
    f.ignore(1);
    sign = 1;
    break;
  case '-':
    f.ignore(1);
    sign = -1;
    break;
  default:
    break;
  }

  auto digit_chars = collect_integer_digits(radix, f);
  auto c = f.peek();

  if((c == '.') || (!digit_chars.first.empty() && check_decimal_suffix(c))){
    if(radix != 10){
      throw zs_error(printf_string("reader error: non-decimal float is not supported. (radix %d)\n", radix));
    }

    // decimal float
    auto d = zs_stof(f, move(digit_chars.first));
      
    return {Number{d * sign}, Exactness::inexact};
  }

  if(digit_chars.first.empty()){
    throw zs_error("reader error: failed at reading a number's integer part\n");
  }

  auto u1 = zs_stol(radix, digit_chars.first);

  if(c == '/'){
    f.ignore(1);

    // rational
    auto digit_chars_2 = collect_integer_digits(radix, f);
    if(digit_chars_2.first.empty()){
      throw zs_error("reader error: failed at reading a rational number's denominator\n");
    }

    auto u2 = zs_stol(radix, digit_chars_2.first);

    return {Number(sign * static_cast<double>(u1) / static_cast<double>(u2)),
        Exactness::inexact};
  }

  // FIXME: inexact or super-big integer can be fall into float.
  return {Number(sign * u1), digit_chars.second};
}

ParserRet parse_complex(int radix, istream& f){
  const auto first_char = f.peek();

  // treating +i, -i. (dirty part!)
  if(first_char == '+' || first_char == '-'){
    f.get();
    if(f.peek() == 'i'){
      f.ignore(1);
      return {Number{Number::complex_type(0, (first_char == '+') ? 1 : -1)},
          Exactness::inexact};
    }
    f.unget();
  }

  // has real part
  auto real = parse_real_number(radix, f);

  switch(auto c = f.get()){
  case '@': {// polar literal
    auto deg = parse_real_number(radix, f);
        
    return {Number{polar(real.number.coerce<double>(), deg.number.coerce<double>())},
        Exactness::inexact};
  }
  case '+': case '-': {
    const int sign = (c == '+') ? 1 : -1;

    if(f.peek() == 'i'){
      f.ignore(1);
      return {Number{Number::complex_type(real.number.coerce<double>(), sign)},
          Exactness::inexact};
    }
    
    auto imag = parse_real_number(radix, f);
    if(f.get() != 'i'){
      throw zs_error("reader error: failed at reading a complex number's imaginary part.\n");
    }

    return {Number{Number::complex_type(real.number.coerce<double>(), imag.number.coerce<double>() * sign)},
        Exactness::inexact};
  }
  case 'i':
    if(first_char == '+' || first_char == '-'){
      return {Number{Number::complex_type(0, real.number.coerce<double>())},
          Exactness::inexact};
    }else{
      throw zs_error("reader error: failed at reading a complex number. ('i' appeared alone.)\n");
    }
  default:
    f.unget();
    return real;
  }
}

} // namespace

Number parse_number(istream& f, int radix){
  const auto prefix_info = parse_number_prefix(f);

  if(!radix){
    radix = prefix_info.radix;
  }

  const auto r = parse_complex(radix, f);

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

static void print_binary(ostream& f, unsigned long l){
  std::string tmp;

  while(l > 0){
    auto b = l % 2;
    tmp.push_back(b ? '1' : '0');
    l /= 2;
  }

  std::copy(tmp.rbegin(), tmp.rend(), ostreambuf_iterator<char>(f));
}

void print(ostream& f, const Number& n, int radix){
  switch(n.type()){
  case Number::Type::uninitialized:
    f << "(uninitialied number)";
    break;
  case Number::Type::complex: {
    auto z = n.get<Number::complex_type>();
    f << z.real() << showpos << z.imag() << noshowpos;
  }
    break;
  case Number::Type::real:
    f << n.get<Number::real_type>();
    break;
  case Number::Type::integer: {
    auto i = n.get<Number::integer_type>();

    if(radix == 10){
      f << i;
    }else{
      auto is_minus = (i < 0);
      auto u = std::abs(i);

      switch(radix){
      case 8:
        f << "#o";
        if(is_minus) f << '-';
        f << oct << u << dec;
        break;
      case 16:
        f << "#x";
        if(is_minus) f << '-';
        f << hex << u << dec;
        break;
      case 2:
        f << "#b";
        if(is_minus) f << '-';
        print_binary(f, u);
        break;
      default:
        UNEXP_DEFAULT();
      }
    }
    break;
  }
  default:
    UNEXP_DEFAULT();
  }
}

// for migration..
void print(std::ostream& os, Lisp_ptr p, int radix){
  if(p.tag() == Ptr_tag::integer){
    print(os, Number(static_cast<long>(p.get<int>())), radix);
  }else{
    auto n = p.get<Number*>();
    assert(n);
    print(os, *n, radix);
  }
}

Number to_exact(const Number& n){
  switch(n.type()){
  case Number::Type::complex:
    throw zs_error("number error: conversion from complex to exact number is not supprted.\n");
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
