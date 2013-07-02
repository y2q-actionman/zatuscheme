#include <cctype>
#include <climits>
#include <cstdlib>
#include <istream>
#include <stdexcept>
#include <string>

#include "config.h"

#include "lisp_ptr.hh"
#include "rational.hh"
#include "token.hh"
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

bool is_delimiter(char c){
  return isspace(c) || c == '(' || c == ')'
    || c ==  '"' || c == ';' || c == EOF;
}

bool is_special_initial(char c){
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


Lisp_ptr tokenize_identifier(istream& f, int first_char){
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
  return {intern(*vm.symtable, move(s))};
}

static
bool check_character_name(istream& f, const char* str){
  for(const char* c = str; *c; ++c){
    auto get_c = f.get();
    if(get_c != ZS_CASE(*c) || is_delimiter(get_c)){
      return false;
    }
  }
  return true;
}

Lisp_ptr tokenize_character(istream& f){
  auto ret_char = f.get();
  if(ret_char == EOF){
    throw_zs_error({}, "reader error: no char found after '#\\'!\n");
  }

  if(is_delimiter(f.peek())){
    return Lisp_ptr{static_cast<char>(ret_char)};
  }else{
    // check character name
    switch(ret_char){
    case 's': case 'S':
      if(check_character_name(f, "pace")){
        return Lisp_ptr{' '};
      }
      break;
    case 'n': case 'N':
      if(check_character_name(f, "ewline")){
        return Lisp_ptr{'\n'};
      }
      break;
    }

    throw_zs_error({}, "reader error: not supported char name!\n");
  }
}

Lisp_ptr tokenize_string(istream& f){
  decltype(f.get()) c;
  string s;

  while((c = f.get()) != EOF){
    switch(c){
    case '"':
      return Lisp_ptr{zs_new<String>(move(s))};
    case '\\':
      switch(c = f.get()){
      case '"': case '\\':
        s.push_back(c);
        break;
      default:
        throw_zs_error({}, "reader error: unknown string escape '%c' appeared.\n", c);
      }
      break;
    default:
      s.push_back(c);
    }
  }

  throw_zs_error({}, "reader error: not ended string!\n");
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
      throw_zs_error({}, "reader error: unknown number prefix '%c' appeared!\n", c);
    }
    
    if(e_appeared > 1 || r_appeared > 1)
      throw_zs_error({}, "reader error: duplicated number prefix appeared (%c)\n", c);
  }  
  
  return {r, e};
}

int char_to_int(int radix, int c){
  int ret;

  switch(c){
  case 'f': case 'F': ret = 15; break;
  case 'e': case 'E': ret = 14; break;
  case 'd': case 'D': ret = 13; break;
  case 'c': case 'C': ret = 12; break;
  case 'b': case 'B': ret = 11; break;
  case 'a': case 'A': ret = 10; break;
  case '9': ret = 9; break;
  case '8': ret = 8; break;
  case '7': ret = 7; break;
  case '6': ret = 6; break;
  case '5': ret = 5; break;
  case '4': ret = 4; break;
  case '3': ret = 3; break;
  case '2': ret = 2; break;
  case '1': ret = 1; break;
  case '0': ret = 0; break;
  default: ret = -1; break;
  }

  if(ret >= radix) return -1;

  return ret;
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


pair<string, Exactness> collect_integer_digits(istream& f, int radix){
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

  print_zs_warning("passed integer fallen into float (%s)", s.c_str());

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
      throw_zs_error({}, "reader error: no chars found for floating point number.\n");
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
      throw_zs_error({}, "reader error: a number starting with dot should have digits after it.\n");
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
        throw_zs_error({}, "reader error: no number on exporational part\n");
      }
    }
  }

  try{
    return std::stod(s, nullptr);
  }catch(const std::logic_error& err){
    throw_zs_error({}, "reader error: reading floating point number failed: %s\n", err.what());
  }
}

Lisp_ptr parse_real_number(istream& f, int radix){
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

  auto digit_chars = collect_integer_digits(f, radix);
  auto c = f.peek();

  if((c == '.') || (!digit_chars.first.empty() && check_decimal_suffix(c))){
    if(radix != 10){
      throw_zs_error({}, "reader error: non-decimal float is not supported. (radix %d)\n", radix);
    }

    // decimal float
    auto d = zs_stof(f, move(digit_chars.first));
      
    return wrap_number(d * sign);
  }

  if(digit_chars.first.empty()){
    throw_zs_error({}, "reader error: failed at reading a number's integer part\n");
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

  auto digit_chars_2 = collect_integer_digits(f, radix);
  if(digit_chars_2.first.empty()){
    throw_zs_error({}, "reader error: failed at reading a rational number's denominator\n");
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

Lisp_ptr parse_complex(istream& f, int radix){
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
  auto real = parse_real_number(f, radix);

  switch(auto c = f.get()){
  case '@': {// polar literal
    auto deg = parse_real_number(f, radix);
        
    return wrap_number(polar(coerce<double>(real), coerce<double>(deg)));
  }
  case '+': case '-': {
    const int sign = (c == '+') ? 1 : -1;

    if(f.peek() == 'i' || f.peek() == 'I'){
      f.ignore(1);
      return wrap_number(Complex(coerce<double>(real), sign));
    }
    
    auto imag = parse_real_number(f, radix);

    if(f.peek() != 'i' && f.peek() != 'I'){
      throw_zs_error({}, "reader error: failed at reading a complex number's imaginary part.\n");
    }
    f.ignore(1);

    return wrap_number(Complex(coerce<double>(real),
                               coerce<double>(imag) * sign));
  }
  case 'i': case 'I':
    if(first_char == '+' || first_char == '-'){
      return wrap_number(Complex(0, coerce<double>(real)));
    }else{
      throw_zs_error({}, "reader error: failed at reading a complex number. ('i' appeared alone.)\n");
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

  switch(radix){
  case 16: case 10: case 8: case 2:
    break;
  default:
    throw_zs_error(wrap_number(radix), "reader error: not supported radix");
  }

  const auto r = parse_complex(f, radix);
  if(!r){
    throw_zs_error({}, "reader error: cannot be read as number.");
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

Lisp_ptr tokenize(istream& f){
  skip_intertoken_space(f);

  switch(auto c = f.get()){
  case '(':
    return Lisp_ptr{Notation::l_paren};
  case ')':
    return Lisp_ptr{Notation::r_paren};
  case '\'':
    return Lisp_ptr{Notation::quote};
  case '`':
    return Lisp_ptr{Notation::quasiquote};
  case '[':
    return Lisp_ptr{Notation::l_bracket};
  case ']':
    return Lisp_ptr{Notation::r_bracket};
  case '{':
    return Lisp_ptr{Notation::l_brace};
  case '}':
    return Lisp_ptr{Notation::r_brace};
  case '|':
    return Lisp_ptr{Notation::bar};
  case ',': {
    if(f.peek() == '@'){
      f.ignore(1);
      return Lisp_ptr{Notation::comma_at};
    }else{
      return Lisp_ptr{Notation::comma};
    }
  }

  case '.': {
    int dots = 1;
    auto c2 = f.peek();

    // checks number like '.1'
    if(char_to_int(10, c2) != -1){
      f.unget();
      return parse_number(f);
    }      

    // '.' or '...' below
    while((c2 = f.get()) == '.'){
      ++dots;
    }
    f.unget();

    switch(dots){
    case 1:
      return Lisp_ptr{Notation::dot};
    case 3:
      return {intern(*vm.symtable, "...")};
    default:
      throw_zs_error({}, "reader error: %d dots appeared.\n", dots);
    }
  }

  case '"':
    return tokenize_string(f);

  case '+': case '-': {
    if(is_delimiter(f.peek())){
      return {intern(*vm.symtable, string(1, c))};
    }else{
      f.unget();
      return parse_number(f);
    }
  }

  case '#':
    switch(auto sharp_c = f.get()){
    case '(':
      return Lisp_ptr{Notation::vector_paren};
    case 't': case 'T':
      return Lisp_ptr{true};
    case 'f': case 'F':
      return Lisp_ptr{false};
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
      return parse_number(f);
    case '<':
      // cleaning input stream
      do{
        c = f.get();
      }while(c != EOF && c != '>');
      f.unget();

      throw_zs_error({},"reader error: '#<...>' appeared ('not printable object' in this implementation.)\n");
    default:
      throw_zs_error({}, "reader error: unknown sharp syntax '#%c' appeared.\n", sharp_c);
    }

  case EOF:
    return Lisp_ptr{static_cast<char>(EOF)};

  default:
    if(isalpha(c) || is_special_initial(c)){
      return tokenize_identifier(f, c);
    }else if(isdigit(c)){
      f.unget();
      return parse_number(f);
    }else{
      throw_zs_error({}, "reader error: invalid char '%c' appeared.\n", c);
    }
  }
}

const char* stringify(Notation n){
  switch(n){
  case Notation::l_paren:
    return "left parensis";
  case Notation::r_paren:
    return "right parensis";
  case Notation::vector_paren:
    return "vector parensis";
  case Notation::quote:
    return "quote";
  case Notation::quasiquote:
    return "backquote";
  case Notation::comma:
    return "comma";
  case Notation::comma_at:
    return "comma+at";
  case Notation::dot:
    return "dot";
  case Notation::l_bracket:
    return "left bracket";
  case Notation::r_bracket:
    return "right bracket";
  case Notation::l_brace:
    return "left brace";
  case Notation::r_brace:
    return "right brace";
  case Notation::bar:
    return "bar";
  default:
    return "(unknown token notation)";
  }
}
