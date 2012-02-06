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

template<typename IN, typename OUT>
inline
bool eat_sharp(IN& i, OUT& o){
  int sharps = 0;

  while(i.peek() == '#'){
    i.ignore(1);
    o.put('0');
    ++sharps;
  }

  return sharps;
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
    eat_sharp(i, s);

    errno = 0;
    long l = strtol(s.str().c_str(), nullptr, radix);
    if(errno) goto error;

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
  
  while(read_char_func(i.peek())){
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
    sharps_before_dot = eat_sharp(i, s);
  }

  if(i.peek() != '.'){
    goto end; // 1. no frac part
  }
  s.put(i.get());
    
  if(sharps_before_dot > 0){
    eat_sharp(i, s);
    goto end; // 4. no frac -- sharps before dot
  }

  if(dot_start && read_char_func(i.peek()) < 0)
    goto error; // 2. dot start should have digits

  while(read_char_func(i.peek())){
    s.put(i.get());
  }

  eat_sharp(i, s);
  
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

    while(read_char_func(i.peek())){
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
  int sign = 1;

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

  const auto unsigned_pos = i.tellg();


  auto u1 = parse_unsigned<radix>(i);
  if(!u1)
    goto error;

  // decimal float
  if(radix == 10){
    i.clear();
    auto next = i.peek();

    if(check_decimal_suffix(next)
       || next == '.' || next == '#'){
      i.seekg(unsigned_pos);
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
    if(!u2)
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
  const auto first_char = i.peek();

  // has real part
  Number real = parse_real_number<radix>(i);
  if(!real)
    goto error;

  switch(auto c = i.peek()){
  case '@': {// polar literal
    i.ignore(1);
    Number deg = parse_real_number<radix>(i);

    if(!deg)
      goto error;
        
    return Number{polar(real.get<double>(), deg.get<double>())};
  }
  case '+': case '-': {
    i.ignore(1);
    const int sign = (c == '+') ? 1 : -1;

    if(i.peek() == 'i'){
      i.ignore(1);
      return Number{Number::complex_type(real.get<double>(), sign)};
    }
    
    Number imag = parse_real_number<radix>(i);
    if(!imag || i.get() != 'i')
      return Number{}; // error

    return Number{Number::complex_type{real.get<double>(), imag.get<double>() * sign}};
  }
  case 'i':
    i.ignore(1);
    if(first_char == '+' || first_char == '-'){
      return Number{Number::complex_type{0, real.get<double>()}};
    }else{
      goto error;
    }
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

void describe(FILE* f, Number::Type t){
  fputs(stringify(t), f);
}

void describe(FILE* f, const Number& n){
  const auto t = n.type();

  fprintf(f, "Number: %s(", stringify(t));

  switch(t){
  case Number::Type::uninitialized:
    break;
  case Number::Type::complex: {
    const auto& z = n.get<Number::complex_type>();
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
    break;
  }

  fputc(')', f);
}

