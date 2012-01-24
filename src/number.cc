#include <istream>
#include <utility>
#include <cctype>
#include <sstream>
#include <cstdlib>

#include "number.hh"

using namespace std;

namespace {

enum class Exactness{
  exact, inexact, unspecified
    };

template<typename CharT>
inline constexpr
int to_radix(CharT c){
  return (c == 'b') ? 2
    : (c == 'o') ? 8
    : (c == 'd') ? 10
    : (c == 'x') ? 16
    : -1;
}

template<typename CharT>
inline constexpr
Exactness to_exactness(CharT c){
  return (c == 'i') ? Exactness::inexact
    : (c == 'e') ? Exactness::exact
    : Exactness::unspecified;
}

pair<int, Exactness> parse_number_prefix(std::istream& i){
  int r = 10;
  Exactness e = Exactness::unspecified;
  bool r_appeared = false, e_appeared = false;

  if(i.peek() != '#')
    return make_pair(r, e);
  i.ignore(1);

  switch(auto c = i.get()){
  case 'i': case 'e':
    e = to_exactness(c);
    e_appeared = true;
    break;
  case 'b': case 'o': case 'd': case 'x':
    r = to_radix(c);
    r_appeared = true;
    break;
  default:
    goto error;
  }
  
  if(i.peek() != '#')
    return make_pair(r, e);
  i.ignore(1);

  switch(auto c = i.get()){
  case 'i': case 'e':
    if(e_appeared) goto error;
    e = to_exactness(c);
    break;
  case 'b': case 'o': case 'd': case 'x':
    if(r_appeared) goto error;
    r = to_radix(c);
  default:
    goto error;
  }
  
  return make_pair(r, e);

 error:
  return make_pair(-1, Exactness::unspecified);
}


template<int radix>
struct is_number_char{
  template<typename CharT>
  inline
  bool operator()(CharT c) const;
};

template<>
template<typename CharT>
inline
bool is_number_char<2>::operator()(CharT c) const{
  switch(c){
  case '0': case '1':
    return true;
  default:
    return false;
  }
}

template<>
template<typename CharT>
inline
bool is_number_char<8>::operator()(CharT c) const{
  switch(c){
  case '0': case '1': case '2': case '3':
  case '4': case '5': case '6': case '7':
    return true;
  default:
    return false;
  }
}

template<>
template<typename CharT>
inline
bool is_number_char<10>::operator()(CharT c) const{
  return isdigit(c);
}

template<>
template<typename CharT>
inline
bool is_number_char<16>::operator()(CharT c) const{
  return isxdigit(c);
}

template<typename T>
inline
int ignore_sharp(T& i){
  int sharps = 0;

  while(i.peek() == '#'){
    i.ignore(1);
    ++sharps;
  }

  return sharps;
}

inline
bool is_inited(const Number& n){
  return n.type() != Number::Type::uninitialized;
}

template<int radix>
Number parse_unsigned(std::istream& i){
  static const auto fun = is_number_char<radix>{};
  const auto pos = i.tellg();
  stringstream s;

  while(fun(i.peek()))
    s.put(i.get());

  if(s.str().empty()){
    goto error;
  }else{
    errno = 0;
    long l = strtol(s.str().c_str(), nullptr, radix);
    if(errno) goto error;

    ignore_sharp(i);

    return Number{l};
  }

 error:
  i.clear();
  i.seekg(pos);
  return Number{};
}

template<typename CharT>
inline
bool check_decimal_suffix(CharT c){
  switch(c){
  case 'e': case 's': case 'f': case 'd': case 'l':
    return true;
  default:
    return false;
  }
}

Number parse_decimal(std::istream& i){
  static const auto read_char_func = is_number_char<10>{};
  const auto pos = i.tellg();

  stringstream s;
  decltype(i.peek()) c;
  
  while((c = read_char_func(i.peek())) >= 0){
    s.put(i.get());
  }

  bool dot_start = false;
  int sharps_before_dot = 0;

  if(s.str().empty()){
    if(i.peek() == '.'){
      dot_start = true;
    }else{
      goto error;
    }
  }else{
    sharps_before_dot = ignore_sharp(i);
  }

  if(i.peek() != '.'){
    goto end; // 1. no frac part
  }
  s.put(i.get());
    
  if(sharps_before_dot > 0){
    ignore_sharp(i);
    goto end; // 4. no frac -- sharps before dot
  }

  if(dot_start && read_char_func(i.peek()) < 0)
    goto error; // 2. dot start should have digits

  while((c = read_char_func(i.peek())) >= 0){
    s.put(i.get());
  }

  ignore_sharp(i);
  
 end:
  if(check_decimal_suffix(i.peek()) < 0){
    i.ignore(1);
    s.put('e');

    switch(i.peek()){
    case '+': case '-':
      s.put(i.get());
    }

    if(read_char_func(i.peek()) < 0){
      goto error; // no number on exp. part
    }

    while((c = read_char_func(i.peek())) >= 0){
      s.put(i.get());
    }
  }

  return Number{strtod(s.str().c_str(), nullptr)};

 error:
  i.clear();
  i.seekg(pos);
  return Number{};
}

template<int radix>
Number parse_real_number(std::istream& i){
  const auto pos = i.tellg();
  int sign = 0;

  switch(i.peek()){
  case '+':
    sign = 1;
    i.ignore(1);
    break;
  case '-':
    sign = -1;
    i.ignore(1);
    break;
  default:
    sign = 1;
    break;
  }

  auto u1 = parse_unsigned<radix>(i);
  if(!is_inited(u1))
    goto error;

  // decimal float
  if(radix == 10){
    i.clear();
    if(check_decimal_suffix(i.peek())){
      i.seekg(pos);
      auto n = parse_decimal(i);

      if(n.type() == Number::Type::real){
        return Number{n.get<double>() * sign};
      }
    }
  }

  if(i.peek() != '/'){
  // integer
    return Number(sign * u1.get<long>());
  }else{
    // rational
    auto u2 = parse_unsigned<radix>(i);
    if(!is_inited(u2))
      goto error;
    return Number(sign * u1.get<double>() / u2.get<double>());
  }

 error:
  i.clear();
  i.seekg(pos);
  return Number{};
}

template<int radix>
Number parse_complex(std::istream& i){
  const auto imag_parser = [&](const Number& real,
                               decltype(i.peek()) sign_char) -> Number {
    const int sign = (sign_char == '+') ? 1 : -1;

    i.ignore(1);

    if(i.peek() == 'i')
      return Number{Number::complex_type(real.get<double>(),
                                         static_cast<double>(sign * 1))};
    
    Number imag = parse_unsigned<radix>(i);

    if(!is_inited(imag) || i.peek() != 'i')
      return Number{}; // error

    return Number{Number::complex_type{real.get<double>(), imag.get<double>() * sign}};
  };

  const auto pos = i.tellg();
  auto first_char = i.peek();

  // no real part
  if(first_char == '+' || first_char == '-'){
    Number n = imag_parser(Number{static_cast<long>(0)}, first_char);

    if(is_inited(n))
      return n;

    i.clear();
    i.seekg(pos);
  }

  // has real part
  Number real = parse_real_number<radix>(i);
  if(!is_inited(real))
    goto error;

  switch(auto c = i.peek()){
  case '@': {// polar literal
    i.ignore(1);
    Number deg = parse_real_number<radix>(i);

    if(!is_inited(deg))
      goto error;
        
    return Number{polar(real.get<double>(), deg.get<double>())};
  }
  case '+': case '-':
    return imag_parser(real, c);
  default:
    return real;
  }

 error:
  return Number{};
}

} // namespace

Number parse_number(std::istream& i){
  const auto pos = i.tellg();
  const auto prefix_info = parse_number_prefix(i);

  Number n;

  switch(get<0>(prefix_info)){
  case 2:
    n = parse_complex<2>(i);
    break;
  case 8:
    n = parse_complex<8>(i);
    break;
  case 10:
    n = parse_complex<10>(i);
    break;
  case 16:
    n = parse_complex<16>(i);
  default:
    goto error;
  }

  switch(get<1>(prefix_info)){
  case Exactness::exact:
    return to_exact(n);
  case Exactness::inexact:
    return to_inexact(n);
  case Exactness::unspecified:
    return n;
  default:
    goto error;
  }

 error:
  i.clear();
  i.seekg(pos);
  return Number{};
}

Number to_exact(const Number& n){
  switch(n.type()){
  case Number::Type::complex:
    return Number{}; // not supported
  case Number::Type::real:
    return Number{static_cast<Number::real_type>
        (n.get<Number::integer_type>())};
  case Number::Type::integer:
    return n;
  case Number::Type::uninitialized:
  default:
    return Number{};
  }
}

Number to_inexact(const Number& n){
  switch(n.type()){
  case Number::Type::complex:
    return n;
  case Number::Type::real:
    return n;
  case Number::Type::integer:
    return Number{static_cast<Number::integer_type>
        (n.get<Number::real_type>())};
  case Number::Type::uninitialized:
  default:
    return Number{};
  }
}

namespace{

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
    return "";
  }
}

} // namespace

void describe(std::ostream& o, Number::Type t){
  o << stringify(t);
}

void describe(std::ostream& o, const Number& n){
  const auto t = n.type();

  o << "Number: " << stringify(t);

  o << "(";
  switch(t){
  case Number::Type::uninitialized:
    break;
  case Number::Type::complex:
    o << n.get<Number::complex_type>();
    break;
  case Number::Type::real:
    o << n.get<Number::real_type>();
    break;
  case Number::Type::integer:
    o << n.get<Number::integer_type>();
    break;
  default:
    break;
  }    
  o << ")";
}

