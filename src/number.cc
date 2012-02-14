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

pair<int, Exactness> parse_number_prefix(std::istream& i){
  static const auto to_radix = [](char c){
    return (c == 'b') ? 2
    : (c == 'o') ? 8
    : (c == 'd') ? 10
    : (c == 'x') ? 16
    : -1;
  };

  static const auto to_exactness = [](char c){
    return (c == 'i') ? Exactness::inexact
    : (c == 'e') ? Exactness::exact
    : Exactness::unspecified;
  };

  int r = 10;
  Exactness e = Exactness::unspecified;
  bool r_appeared = false, e_appeared = false;

  if(i.peek() != '#')
    return {r, e};
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
    return {r, e};
  i.ignore(1);

  switch(auto c = i.get()){
  case 'i': case 'e':
    if(e_appeared) goto error;
    e = to_exactness(c);
    break;
  case 'b': case 'o': case 'd': case 'x':
    if(r_appeared) goto error;
    r = to_radix(c);
    break;
  default:
    goto error;
  }
  
  return {r, e};

 error:
  return {-1, Exactness::unspecified};
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
int eat_sharp(IN& i, OUT& o){
  int sharps = 0;

  while(i.peek() == '#'){
    i.ignore(1);
    o.put('0');
    ++sharps;
  }

  return sharps;
}


typedef pair<Number, Exactness> ParserRet;

#define PARSE_ERROR_VALUE (ParserRet{{}, Exactness::unspecified})

template<int radix>
ParserRet parse_unsigned(std::istream& i, stringstream& s){
  static const auto fun = is_number_char<radix>{};

  while(fun(i.peek()))
    s.put(i.get());

  if(s.str().empty()){
    goto error;
  }else{
    Exactness e;

    if(eat_sharp(i, s) > 0){
      e = Exactness::inexact;
    }else{
      e = Exactness::exact;
    }

    errno = 0;
    long l = strtol(s.str().c_str(), nullptr, radix);
    if(errno) goto error;

    return {Number{l}, e};
  }

 error:
  return PARSE_ERROR_VALUE;
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

ParserRet parse_decimal(std::istream& i, stringstream& s){
  static const auto read_char_func = is_number_char<10>{};

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
    goto end; // 4. only sharps after dot
  }

  if(dot_start && !read_char_func(i.peek()))
    goto error; // 2. dot start should have digits

  while(read_char_func(i.peek())){
    s.put(i.get());
  }

  eat_sharp(i, s);
  
 end:
  if(check_decimal_suffix(i.peek())){
    i.ignore(1);
    s.put('e');

    switch(i.peek()){
    case '+': case '-':
      s.put(i.get());
    }

    if(!read_char_func(i.peek())){
      goto error; // no number on exp. part
    }

    while(read_char_func(i.peek())){
      s.put(i.get());
    }
  }

  return {Number{strtod(s.str().c_str(), nullptr)},
      Exactness::inexact};

 error:
  return PARSE_ERROR_VALUE;
}

template<int radix>
ParserRet parse_real_number(std::istream& i){
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

  stringstream s;

  auto u1 = parse_unsigned<radix>(i, s);
  if(!get<0>(u1)){
    if(i.peek() == '.'){
      goto decimal_float_check;
    }else{
      goto error;
    }
  }

  if(check_decimal_suffix(i.peek()) || i.peek() == '.'){
  decimal_float_check:
    if(radix == 10){
      auto n = parse_decimal(i, s);

      if(get<0>(n).type() == Number::Type::real){
        return {Number{get<0>(n).get<double>() * sign},
            Exactness::inexact};
      }
    }
    goto error;
  }

  if(i.peek() != '/'){ // integer?
    // FIXME: inexact or super-big integer can be fall into float.
    return {Number(sign * get<0>(u1).get<long>()), get<1>(u1)};
  }else{
    // rational
    stringstream s2;
    auto u2 = parse_unsigned<radix>(i, s2);
    if(!get<0>(u2))
      goto error;
    return {Number(sign * get<0>(u1).get<double>() / get<0>(u2).get<double>()),
        Exactness::inexact};
  }

 error:
  return PARSE_ERROR_VALUE;
}

template<int radix>
ParserRet parse_complex(std::istream& i){
  const auto first_char = i.peek();

  // has real part
  auto real = parse_real_number<radix>(i);
  if(!get<0>(real))
    goto error;

  switch(auto c = i.peek()){
  case '@': {// polar literal
    i.ignore(1);
    auto deg = parse_real_number<radix>(i);

    if(!get<0>(deg))
      goto error;
        
    return {Number{polar(get<0>(real).get<double>(), get<0>(deg).get<double>())},
        Exactness::inexact};
  }
  case '+': case '-': {
    i.ignore(1);
    const int sign = (c == '+') ? 1 : -1;

    if(i.peek() == 'i'){
      i.ignore(1);
      return {Number{get<0>(real).get<double>(), static_cast<double>(sign)},
          Exactness::inexact};
    }
    
    auto imag = parse_real_number<radix>(i);
    if(!get<0>(imag) || i.get() != 'i')
      goto error;

    return {Number{get<0>(real).get<double>(), get<0>(imag).get<double>() * sign},
        Exactness::inexact};
  }
  case 'i':
    i.ignore(1);
    if(first_char == '+' || first_char == '-'){
      return {Number{0, get<0>(real).get<double>()},
          Exactness::inexact};
    }else{
      goto error;
    }
  default:
    return real;
  }

 error:
  return PARSE_ERROR_VALUE;
}

} // namespace

Number parse_number(std::istream& i){
  const auto prefix_info = parse_number_prefix(i);

  ParserRet r;

  switch(get<0>(prefix_info)){
  case 2:
    r = parse_complex<2>(i);
    break;
  case 8:
    r = parse_complex<8>(i);
    break;
  case 10:
    r = parse_complex<10>(i);
    break;
  case 16:
    r = parse_complex<16>(i);
    break;
  default:
    goto error;
  }

  if(!get<0>(r)) goto error;

  // TODO: check inexact integer, and warn.

  switch(auto e = get<1>(prefix_info)){
  case Exactness::exact:
    return (get<1>(r) == e) ? get<0>(r) : to_exact(get<0>(r));
  case Exactness::inexact:
    return (get<1>(r) == e) ? get<0>(r) : to_inexact(get<0>(r));
  case Exactness::unspecified:
    return get<0>(r);
  default:
    goto error;
  }

 error:
  return {};
}

Number to_exact(const Number& n){
  switch(n.type()){
  case Number::Type::complex:
    return {}; // not supported
  case Number::Type::real:
    return {}; // not supported
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
    return Number{static_cast<Number::real_type>
        (n.get<Number::integer_type>())};
  case Number::Type::uninitialized:
  default:
    return {};
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

