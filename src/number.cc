#include <istream>
#include <utility>
#include <cctype>
#include <sstream>

#include "number.hh"

using namespace std;

enum class Exactness{
  exact, inexact, unspecified
    };

template<typename CharT>
static inline constexpr
int to_radix(CharT c){
  return (c == 'b') ? 2
    : (c == 'o') ? 8
    : (c == 'd') ? 10
    : (c == 'x') ? 16
    : -1;
}

template<typename CharT>
static inline constexpr
Exactness to_exactness(CharT c){
  return (c == 'i') ? Exactness::inexact
    : (c == 'e') ? Exactness::exact
    : Exactness::unspecified;
}

static
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
    throw "informal char appeared!";
  }
  
  if(i.peek() != '#')
    return make_pair(r, e);
  i.ignore(1);

  switch(auto c = i.get()){
  case 'i': case 'e':
    if(e_appeared) throw "exactness specified twice";
    e = to_exactness(c);
    break;
  case 'b': case 'o': case 'd': case 'x':
    if(r_appeared) throw "radix specified twice";
    r = to_radix(c);
  default:
    throw "informal char appeared!";
  }
  
  return make_pair(r, e);
}


static inline
int char_to_int(char c){
  switch(c){
  case '0': return 0;
  case '1': return 1;
  case '2': return 2;
  case '3': return 3;
  case '4': return 4;
  case '5': return 5;
  case '6': return 6;
  case '7': return 7;
  case '8': return 8;
  case '9': return 9;
  case 'a': case 'A': return 10;
  case 'b': case 'B': return 11;
  case 'c': case 'C': return 12;
  case 'd': case 'D': return 13;
  case 'e': case 'E': return 14;
  case 'f': case 'F': return 15;
  default: return -1;
  }
}

template<int radix>
struct is_number_char{
  template<typename CharT>
  inline
  int operator()(CharT c) const;
};

template<>
template<typename CharT>
inline
int is_number_char<2>::operator()(CharT c) const{
  switch(c){
  case '0': case '1':
    return char_to_int(c);
  default:
    return -1;
  }
}

template<>
template<typename CharT>
inline
int is_number_char<8>::operator()(CharT c) const{
  switch(c){
  case '0': case '1': case '2': case '3':
  case '4': case '5': case '6': case '7':
    return char_to_int(c);
  default:
    return -1;
  }
}

template<>
template<typename CharT>
inline
int is_number_char<10>::operator()(CharT c) const{
  return isdigit(c) ? char_to_int(c) : -1;
}

template<>
template<typename CharT>
inline
int is_number_char<16>::operator()(CharT c) const{
  return isxdigit(c) ? char_to_int(c) : -1;
}


template<int radix>
static
Number parse_unsigned(std::istream& i){
  auto fun = is_number_char<radix>{};
  unsigned long ret = 0;

  while(1){
    ret *= radix;

    auto n = fun(i.peek());
    if(n < 0){
      break;
    }else{
      i.ignore(1);
      ret += n;
    }
  }

  while(1){
    auto c = i.peek();
    if(c != '#') break;
    i.ignore(1);
  }

  return Number{static_cast<long>(ret)};
}

template<typename CharT>
static inline
bool check_decimal_suffix(CharT c){
  switch(c){
  case 'e': case 's': case 'f': case 'd': case 'l':
    return true;
  default:
    return false;
  }
}

static
Number parse_decimal(std::istream& i){
  static const auto read_char_func = is_number_char<10>{};
  const auto pos = i.tellg();

  const auto ignore_sharp = [&]() -> int{
    int sharps = 0;
    
    while(i.peek() == '#'){
      i.ignore(1);
      ++sharps;
    }

    return sharps;
  };
  
  stringstream s;
  decltype(i.peek()) c;
  
  // int_part
  while((c = read_char_func(i.peek())) >= 0){
    s.put(i.get());
  }

  bool dot_start = false;
  int sharps = 0;

  if(s.str().empty()){
    if(i.peek() == '.'){
      dot_start = true;
    }else{
      goto error;
    }
  }else{
    sharps = ignore_sharp();
  }

  if(i.peek() != '.'){
    if(check_decimal_suffix(i.peek()))
      goto end;
    else
      goto error;
  }
  s.put(i.get());
    
  if(sharps > 0){
    ignore_sharp();

    if(check_decimal_suffix(i.peek()))
      goto end;
    else
      goto error;
  }

  if(dot_start && read_char_func(i.peek()) < 0)
    goto error;

  while((c = read_char_func(i.peek())) >= 0){
    s.put(i.get());
  }

  ignore_sharp();
  
  if(read_char_func(i.peek()) < 0)
    goto error;

 end: {
    double d;
    s >> d;
    return Number{d};
  }

 error:
  i.seekg(pos);
  return Number{};
}

template<int radix>
static
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

  if(radix == 10 && check_decimal_suffix(i.peek())){
    i.seekg(pos);
    auto n = parse_decimal(i);

    if(n.type() == Number::Type::real){
      return Number{n.get<double>() * sign};
    }
  }

  if(i.peek() != '/'){
    return Number(sign * u1.get<long>());
  }else{
    auto u2 = parse_unsigned<radix>(i);
    return Number(sign * u1.get<double>() / u2.get<double>());
  }
}

template<int radix>
static
Number parse_complex(std::istream& i){
  auto imag_parser = [&](const Number& real,
                         decltype(i.peek()) sign_char) -> Number {
    const int sign = (sign_char == '+') ? 1 : -1;

    i.ignore(1);

    if(i.peek() == 'i')
      return Number{Number::complex_type(real.get<double>(),
                                         static_cast<double>(sign * 1))};
    
    Number imag = parse_unsigned<radix>(i);

    if(imag.type() == Number::Type::uninitialized
       || i.peek() != 'i')
      return Number{}; // error

    return Number{Number::complex_type{real.get<double>(), imag.get<double>() * sign}};
  };

  auto first_char = i.peek();

  // no real part
  if(first_char == '+' || first_char == '-'){
    auto pos = i.tellg();

    Number n = imag_parser(Number{static_cast<long>(0)}, first_char);

    if(n.type() != Number::Type::uninitialized)
      return n;

    i.seekg(pos);
  }

  // has real part
  Number real = parse_real_number<radix>(i);
  if(real.type() == Number::Type::uninitialized)
    return Number{};

  switch(auto c = i.peek()){
  case '@': {// polar literal
    i.ignore(1);
    Number deg = parse_real_number<radix>(i);

    if(real.type() == Number::Type::uninitialized)
      return Number{};
        
    return Number{polar(real.get<double>(), deg.get<double>())};
  }
  case '+': case '-':
    return imag_parser(real, c);
  default:
    return real;
  }
}

Number parse_number(std::istream& i){
  auto prefix_info = parse_number_prefix(i);

  Number n;

  switch(prefix_info.first){
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
    throw "internal parser error!";
  }

  switch(prefix_info.second){
  case Exactness::exact:
    return to_exact(n);
  case Exactness::inexact:
    return to_inexact(n);
  case Exactness::unspecified:
    return n;
  default:
    throw "internal parser error!";
  }
}
