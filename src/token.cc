#include <istream>
#include <cstdlib>
#include <cstring>
#include <stdexcept>
#include <climits>
#include <iostream>
#include <cctype>

#include "config.h"

#include "token.hh"
#include "rational.hh"
#include "zs_error.hh"
#include "zs_memory.hh"
#include "vm.hh"

#ifdef USE_CASE_UPPER
# define ZS_CASE(c) toupper(c)
#elif USE_CASE_LOWER
# define ZS_CASE(c) tolower(c)
#else
# define ZS_CASE(c) c
#endif

using namespace std;

namespace {

template<typename charT>
inline constexpr
bool is_delimiter(charT c){
  return isspace(c) || c == '(' || c == ')'
    || c ==  '"' || c == ';' || c == EOF;
}

template<typename charT>
inline
bool is_special_initial(charT c){
  switch(c){
  case '!': case '$': case '%': case '&':
  case '*': case '/': case ':': case '<':
  case '=': case '>': case '?': case '^':
  case '_': case '~':
    return true;
  default:
    return false;
  }
}

void skip_intertoken_space(istream& f){
  decltype(f.get()) c;
    
  while((c = f.get()) != EOF
        && (isspace(c) || c == ';')){
    if(c == ';'){
      do{
        c = f.get();
      }while(c != EOF && c != '\n');
    }
  }
  f.unget();
}


Token tokenize_identifier(istream& f, int first_char){
  string s(1, ZS_CASE(first_char));

  // subsequent
  decltype(f.get()) c;

  while(!is_delimiter(c = f.get())
        && (isalpha(c) || is_special_initial(c) 
            || isdigit(c) || c == '+' || c == '-'
            || c == '.' || c == '@')){
    s.push_back(ZS_CASE(c));
  }
  f.unget();

  assert(!s.empty());
  return Token{Lisp_ptr{intern(*vm.symtable, move(s))}};
}

Token tokenize_character(istream& f){
  // function for checking character-name
  const auto check_name = [&](const char* str) -> bool {
    for(const char* c = str; *c; ++c){
      auto get_c = f.get();
      if(get_c != tolower(*c) || is_delimiter(get_c)){
        return false;
      }
    }
    return true;
  };

  auto ret_char = f.get();
  if(ret_char == EOF){
    throw zs_error("reader error: no char found after '#\\'!\n");
  }

  if(is_delimiter(f.peek())){
    return Token{Lisp_ptr(static_cast<char>(ret_char))};
  }else{
    // check character name
    switch(ret_char){
    case 's': case 'S':
      if(check_name("pace")){
        return Token{Lisp_ptr(' ')};
      }
      break;
    case 'n': case 'N':
      if(check_name("ewline")){
        return Token{Lisp_ptr('\n')};
      }
      break;
    }

    throw zs_error("reader error: not supported char name!\n");
  }
}

Token tokenize_string(istream& f){
  decltype(f.get()) c;
  string s;

  while((c = f.get()) != EOF){
    switch(c){
    case '"':
      return Token{Lisp_ptr(zs_new<String>(move(s)))};
    case '\\':
      switch(c = f.get()){
      case '"': case '\\':
        s.push_back(c);
        break;
      default:
        throw zs_error(printf_string("reader error: unknown string escape '%c' appeared.\n", c));
      }
      break;
    default:
      s.push_back(c);
    }
  }

  throw zs_error("reader error: not ended string!\n");
}

  // number parsers
enum class Exactness{ unspecified, exact, inexact };

pair<int, Exactness> parse_number_prefix(istream& f){
  int r = 10;
  Exactness e = Exactness::unspecified;
  int r_appeared = 0, e_appeared = 0;

  for(int loop = 0; loop < 2; ++loop){
    auto c = f.get();

    if(c != '#'){
      f.unget();
      return {r, e};
    }

    switch(c = f.get()){
    case 'i': case 'I':
      ++e_appeared;
      e = Exactness::inexact;
      break;
    case 'e': case 'E':
      ++e_appeared;
      e = Exactness::exact;
      break;
    case 'b': case 'B':
      ++r_appeared;
      r = 2;
      break;
    case 'o': case 'O':
      ++r_appeared;
      r = 8;
      break;
    case 'd': case 'D':
      ++r_appeared;
      r = 10;
      break;
    case 'x': case 'X':
      ++r_appeared;
      r = 16;
      break;
    default:
      throw zs_error(printf_string("reader error: unknown number prefix '%c' appeared!\n", c));
    }
    
    if(e_appeared > 1)
      throw zs_error(printf_string("reader error: duplicated number prefix appeared (%c)\n", c));

    if(r_appeared > 1)
      throw zs_error(printf_string("reader error: duplicated number prefix appeared (%c)\n", c));
  }  
  
  return {r, e};
}

int char_to_int(int radix, int c){
  switch(radix){
  case 16: goto xdigit;
  case 10: goto digit;
  case 8:  goto octet;
  case 2:  goto binary;
  default: UNEXP_DEFAULT();
  }

 xdigit:
  if(c == 'f' || c == 'F') return 15;
  if(c == 'e' || c == 'E') return 14;
  if(c == 'd' || c == 'D') return 13;
  if(c == 'c' || c == 'C') return 12;
  if(c == 'b' || c == 'B') return 11;
  if(c == 'a' || c == 'A') return 10;
 digit:
  if(c == '9') return 9;
  if(c == '8') return 8;
 octet:
  if(c == '7') return 7;
  if(c == '6') return 6;
  if(c == '5') return 5;
  if(c == '4') return 4;
  if(c == '3') return 3;
  if(c == '2') return 2;
 binary:
  if(c == '1') return 1;
  if(c == '0') return 0;

  return -1;
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


pair<string, Exactness> collect_integer_digits(int radix, istream& f){
  decltype(f.get()) c;
  string s;

  while(char_to_int(radix, c = f.get()) != -1)
    s.push_back(c);
  f.unget();

  auto sharps = eat_sharp(f, s);
  return {s, (sharps > 0) ? Exactness::inexact : Exactness::exact};
}
  
Lisp_ptr zs_stoi(int radix, const string& s){
  if(s.empty()){
    return wrap_number(0);
  }

  long long tmp = 0;
  auto i = begin(s), e = end(s);

  while(1){
    auto digit = char_to_int(radix, *i);
    if(digit == -1){
      return wrap_number(static_cast<int>(tmp));
    }

    tmp += digit;

    ++i;
    if(i == e){
      return wrap_number(static_cast<int>(tmp));
    }

    tmp *= radix;
    if(tmp > INT_MAX) break;
  }

  cerr << "passed integer fallen into float (" << s << ")\n";

  double d = static_cast<double>(tmp);

  while(1){
    auto digit = char_to_int(radix, *i);
    ++i;

    if(digit == -1 || i == e){
      return wrap_number(d);
    }

    d += digit;
    d *= 10;
  }

  UNEXP_DEFAULT();
}

bool check_decimal_suffix(int c){
  switch(c){
  case 'e': case 'E':
  case 's': case 'S':
  case 'f': case 'F':
  case 'd': case 'D':
  case 'l': case 'L':
    return true;
  default:
    return false;
  }
}

double zs_stof(istream& f, string s){
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

Lisp_ptr parse_real_number(int radix, istream& f){
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
      
    return wrap_number(d * sign);
  }

  if(digit_chars.first.empty()){
    throw zs_error("reader error: failed at reading a number's integer part\n");
  }

  auto u1 = zs_stoi(radix, digit_chars.first);
  if(digit_chars.second == Exactness::inexact){
    u1 = wrap_number(coerce<double>(u1));
  }

  if(c != '/'){
    if(u1.tag() == Ptr_tag::integer){
      return wrap_number(coerce<int>(u1) * sign);
    }else{
      return wrap_number(coerce<double>(u1) * sign);
    }
  }

  // rational
  f.ignore(1);

  auto digit_chars_2 = collect_integer_digits(radix, f);
  if(digit_chars_2.first.empty()){
    throw zs_error("reader error: failed at reading a rational number's denominator\n");
  }

  auto u2 = zs_stoi(radix, digit_chars_2.first);
  if(digit_chars_2.second == Exactness::inexact){
    u2 = wrap_number(coerce<double>(u2));
  }

  if(u1.tag() == Ptr_tag::integer && u2.tag() == Ptr_tag::integer){
    return wrap_number(Rational(sign * coerce<int>(u1), coerce<int>(u2)));
  }else{
    return wrap_number(sign * coerce<double>(u1) / coerce<double>(u2));
  }
}

Lisp_ptr  parse_complex(int radix, istream& f){
  const auto first_char = f.peek();

  // treating +i, -i. (dirty part!)
  if(first_char == '+' || first_char == '-'){
    f.get();
    if(f.peek() == 'i' || f.peek() == 'I'){
      f.ignore(1);
      return wrap_number(Complex(0, (first_char == '+') ? 1 : -1));
    }
    f.unget();
  }

  // has real part
  auto real = parse_real_number(radix, f);

  switch(auto c = f.get()){
  case '@': {// polar literal
    auto deg = parse_real_number(radix, f);
        
    return wrap_number(polar(coerce<double>(real), coerce<double>(deg)));
  }
  case '+': case '-': {
    const int sign = (c == '+') ? 1 : -1;

    if(f.peek() == 'i' || f.peek() == 'I'){
      f.ignore(1);
      return wrap_number(Complex(coerce<double>(real), sign));
    }
    
    auto imag = parse_real_number(radix, f);

    if(f.peek() != 'i' && f.peek() != 'I'){
      throw zs_error("reader error: failed at reading a complex number's imaginary part.\n");
    }
    f.ignore(1);

    return wrap_number(Complex(coerce<double>(real),
                               coerce<double>(imag) * sign));
  }
  case 'i': case 'I':
    if(first_char == '+' || first_char == '-'){
      return wrap_number(Complex(0, coerce<double>(real)));
    }else{
      throw zs_error("reader error: failed at reading a complex number. ('i' appeared alone.)\n");
    }
  default:
    f.unget();
    return real;
  }
}

} // namespace

Lisp_ptr parse_number(istream& f, int radix){
  const auto prefix_info = parse_number_prefix(f);

  if(!radix){
    radix = prefix_info.first;
  }

  const auto r = parse_complex(radix, f);
  if(!r){
    // TODO: add context information.
    throw zs_error("parser error: cannot be read as number.\n");
  }

  switch(prefix_info.second){
  case Exactness::unspecified:
    return r;
  case Exactness::exact:
    return to_exact(r);
  case Exactness::inexact:
    return to_inexact(r);
  default:
    UNEXP_DEFAULT();
  }
}

Token tokenize(istream& f){
  skip_intertoken_space(f);

  switch(auto c = f.get()){
  case '(':
    return Token{Token::Notation::l_paren};
  case ')':
    return Token{Token::Notation::r_paren};
  case '\'':
    return Token{Token::Notation::quote};
  case '`':
    return Token{Token::Notation::quasiquote};
  case '[':
    return Token{Token::Notation::l_bracket};
  case ']':
    return Token{Token::Notation::r_bracket};
  case '{':
    return Token{Token::Notation::l_brace};
  case '}':
    return Token{Token::Notation::r_brace};
  case '|':
    return Token{Token::Notation::bar};
  case ',': {
    if(f.peek() == '@'){
      f.ignore(1);
      return Token{Token::Notation::comma_at};
    }else{
      return Token{Token::Notation::comma};
    }
  }

  case '.': {
    int dots = 1;
    auto c2 = f.peek();

    // checks number like '.1'
    if(char_to_int(10, c2) != -1){
      f.unget();
      return Token{parse_number(f)};
    }      

    // '.' or '...' below
    while((c2 = f.get()) == '.'){
      ++dots;
    }
    f.unget();

    switch(dots){
    case 1:
      return Token{Token::Notation::dot};
    case 3:
      return Token{Lisp_ptr{intern(*vm.symtable, "...")}};
    default:
      throw zs_error(printf_string("reader error: %d dots appeared.\n", dots));
    }
  }

  case '"':
    return tokenize_string(f);

  case '+': case '-': {
    if(is_delimiter(f.peek())){
      return Token{Lisp_ptr{intern(*vm.symtable, string(1, c))}};
    }else{
      f.unget();
      return Token{parse_number(f)};
    }
  }

  case '#':
    switch(auto sharp_c = f.get()){
    case '(':
      return Token{Token::Notation::vector_paren};
    case 't': case 'T':
      return Token{Lisp_ptr(true)};
    case 'f': case 'F':
      return Token{Lisp_ptr(false)};
    case '\\':
      return tokenize_character(f);
    case 'i': case 'I':
    case 'b': case 'B':
    case 'd': case 'D':
    case 'e': case 'E':
    case 'o': case 'O':
    case 'x': case 'X':
      f.unget();
      f.putback('#');
      return Token{parse_number(f)};
    case '<':
      // cleaning input stream
      do{
        c = f.get();
      }while(c != EOF && c != '>');
      f.unget();

      throw zs_error("reader error: '#<...>' appeared ('not printable object' in this implementation.)\n");
    default:
      throw zs_error(printf_string("reader error: unknown sharp syntax '#%c' appeared.\n", sharp_c));
    }

  case EOF:
    return Token{};

  default:
    if(isalpha(c) || is_special_initial(c)){
      return tokenize_identifier(f, c);
    }else if(isdigit(c)){
      f.unget();
      return Token{parse_number(f)};
    }else{
      throw zs_error(printf_string("reader error: invalid char '%c' appeared.\n", c));
    }
  }
}

const char* stringify(Token::Notation n){
  switch(n){
  case Token::Notation::l_paren:
    return "left parensis";
  case Token::Notation::r_paren:
    return "right parensis";
  case Token::Notation::vector_paren:
    return "vector parensis";
  case Token::Notation::quote:
    return "quote";
  case Token::Notation::quasiquote:
    return "backquote";
  case Token::Notation::comma:
    return "comma";
  case Token::Notation::comma_at:
    return "comma+at";
  case Token::Notation::dot:
    return "dot";
  case Token::Notation::l_bracket:
    return "left bracket";
  case Token::Notation::r_bracket:
    return "right bracket";
  case Token::Notation::l_brace:
    return "left brace";
  case Token::Notation::r_brace:
    return "right brace";
  case Token::Notation::bar:
    return "bar";
  default:
    return "(unknown token notation)";
  }
}

const char* stringify(Token::Type t){
  switch(t){
  case Token::Type::uninitialized:
    return "uninitialized";
  case Token::Type::notation:
    return "notation";
  case Token::Type::lisp_ptr:
    return "lisp_ptr";
  default:
    return "(unknown token type)";
  }    
}
