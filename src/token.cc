#include <istream>
#include <sstream>
#include <utility>

#include "token.hh"
#include "util.hh"

using namespace std;

inline
Token::Token(const std::string& s, Type t)
  : type_(t){
  new (&this->str_) std::string(s);
}

inline
Token::Token(std::string&& s, Type t)
  : type_(t){
  new (&this->str_) std::string{std::forward<std::string>(s)};
}

inline
Token::Token(const Number& n)
  : type_(Type::number){
  new (&this->num_) Number(n);
}

inline
Token::Token(Number&& n)
  : type_(Type::number){
  new (&this->num_) Number{std::forward<Number>(n)};
}

inline constexpr
Token::Token(bool b)
  : type_(Type::boolean), b_(b){}

inline constexpr
Token::Token(char c)
  : type_(Type::character), c_(c){}

inline constexpr
Token::Token(Notation n)
  : type_(Type::notation), not_(n){}

template<typename T>
inline
void Token::init_from_other(T other){
  switch(other.type_){
  case Type::uninitialized:
    break;

  case Type::identifier:
  case Type::string:
    new (&this->str_) string(move(other.str_));
    break;

  case Type::boolean:
    this->b_ = other.b_;
    break;

  case Type::number:
    new (&this->num_) Number(move(other.num_));
    break;

  case Type::character:
    this->c_ = other.c_;
    break;

  case Type::notation:
    this->not_ = other.not_;
    break;

  default:
    UNEXP_DEFAULT();
  }
}

Token::Token(const Token& other)
  : type_(other.type_)
{
  init_from_other<const Token&>(forward<const Token>(other));
}
  
Token::Token(Token&& other)
  : type_(other.type_)
{
  init_from_other<Token&&>(forward<Token>(other));
}

template<typename T>
inline
Token& Token::assign_from_other(T other){
  switch(this->type_){
  case Type::uninitialized:
    new (this) Token(move(other));
    break;
    
  case Type::identifier:
  case Type::string:
    if(other.type() == Type::identifier || other.type() == Type::string){
      this->str_ = move(other.str_);
    }else{
      str_.~string();
      new (this) Token(move(other));
    }
    break;

  case Type::boolean:
    if(other.type() == this->type()){
      b_ = other.b_;
    }else{
      new (this) Token(move(other));
    }
    break;

  case Type::number:
    if(other.type() == this->type()){
      this->num_ = move(other.num_);
    }else{
      num_.~Number();
      new (this) Token(move(other));
    }
    break;

  case Type::character:
    if(other.type() == this->type()){
      this->c_ = other.c_;
    }else{
      new (this) Token(move(other));
    }
    break;

  case Type::notation:
    if(other.type() == this->type()){
      this->not_ = move(other.not_);
    }else{
      new (this) Token(move(other));
    }
    break;

  default:
    UNEXP_DEFAULT();
  }

  return *this;
}

Token& Token::operator=(const Token& other){
  return assign_from_other<const Token&>(forward<const Token>(other));
}

Token& Token::operator=(Token&& other){
  return assign_from_other<Token&&>(forward<Token>(other));
}

//
// tokenizer funcs
//

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

void skip_intertoken_space(FILE* f){
  decltype(fgetc(f)) c;
    
  while((c = fgetc(f)) != EOF){
    if(isspace(c)){
      continue;
    }else if(c == ';'){
      do{
        c = fgetc(f);
      }while(c != EOF && c != '\n');
      ungetc(c, f);
    }else{
      ungetc(c, f);
      return;
    }
  }
}


Token tokenize_identifier(FILE* f, char first_char){
  ostringstream s;

  s.put(first_char);

  // subsequent
  auto c = fgetc(f);

  while(!is_delimiter(c) 
        || isalpha(c) || is_special_initial(c) 
        || isdigit(c) || c == '+' || c == '-'
        || c == '.' || c == '@'){
    s.put(c);
    c = fgetc(f);
  }
  ungetc(c, f);

  return Token{s.str(), Token::Type::identifier};
}

Token tokenize_character(FILE* f){
  // function for checking character-name
  const auto check_name = [&](const char* str) -> bool {
    for(const char* c = str; *c; ++c){
      auto get_c = fgetc(f);
      if(get_c != *c || is_delimiter(get_c)){
        ungetc(get_c, f);
        return false;
      }
    }
    return true;
  };

  auto ret_char = fgetc(f);

  // check character name
  switch(ret_char){
  case EOF:
    return {};
  case 's':
    if(check_name("pace")){
      ret_char = ' ';
    }
    break;
  case 'n':
    if(check_name("ewline")){
      ret_char = '\n';
    }
    break;
  }

  return Token{static_cast<char>(ret_char)};
}

Token tokenize_string(FILE* f){
  decltype(fgetc(f)) c;
  ostringstream s;

  while((c = fgetc(f)) != EOF){
    switch(c){
    case '"':
      return Token{s.str(), Token::Type::string};
    case '\\':
      c = fgetc(f);
      switch(c){
      case '"': case '\\':
        s.put(c);
        break;
      default:
        goto error;
      }
      break;
    default:
      s.put(c);
    }
  }

 error:
  return {};
}

Token tokenize_number(FILE* f, char c1, char c2 = 0){
  static const auto is_n_char = [](char c) -> bool {
    switch(c){
    case '+': case '-': case '.': case '@': case 'i':
    case 'e': case 's': case 'f': case 'd': case 'l':
    case '#': case 'b': case 'o': case 'x':
      return true;
    default:
      return isxdigit(c);
    }
  };

  decltype(fgetc(f)) c;
  stringstream s;

  s.put(c1);
  if(c2) s.put(c2);

  while(is_n_char(c = fgetc(f))){
    s.put(c);
  }
  ungetc(c, f);

  if(auto n = parse_number(s)){
    // TODO: push back unused chars.
    return Token{n};
  }else{
    return Token{};
  }
}

} // namespace

Token tokenize(FILE* f){
  skip_intertoken_space(f);

  switch(auto c = fgetc(f)){
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
    auto c2 = fgetc(f);
    if(c2 == '@'){
      return Token{Token::Notation::comma_at};
    }else{
      ungetc(c2, f);
      return Token{Token::Notation::comma};
    }
  }

  case '.': {
    int dots = 1;
    auto c2 = fgetc(f);

    while(c2 == '.'){
      ++dots;
      c2 = fgetc(f);
    }
    ungetc(c2, f);

    switch(dots){
    case 1:
      return Token{Token::Notation::dot};
    case 3:
      return Token{"...", Token::Type::identifier};
    default:
      goto error;
    }
  }

  case '"':
    return tokenize_string(f);

  case '+': case '-': {
    auto c2 = fgetc(f);

    if(is_delimiter(c2)){
      return Token{string(1, c), Token::Type::identifier};
    }else{
      return tokenize_number(f, c, c2);
    }
  }

  case '#':
    switch(auto sharp_c = fgetc(f)){
    case '(':
      return Token{Token::Notation::vector_paren};
    case 't':
      return Token{true};
    case 'f':
      return Token{false};
    case '\\':
      return tokenize_character(f);
    case 'i': case 'e':
    case 'b': case 'o':
    case 'd': case 'x':
      return tokenize_number(f, '#', sharp_c);
    default:
      goto error;
    }

  default:
    if(isalpha(c) || is_special_initial(c)){
      return tokenize_identifier(f, c);
    }else if(isdigit(c)){
      return tokenize_number(f, c);
    }else{
      goto error;
    }
  }

 error:
  return {};
}

const char* stringify(Token::Notation n){
  switch(n){
  case Token::Notation::unknown:
    return "unknown";
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
  case Token::Type::identifier:
    return "identifier";
  case Token::Type::string:
    return "string";
  case Token::Type::boolean:
    return "boolean";
  case Token::Type::number:
    return "number";
  case Token::Type::character:
    return "character";
  case Token::Type::notation:
    return "notation";
  default:
    return "(unknown token type)";
  }    
}

void describe(FILE* f, Token::Type t){
  fputs(stringify(t), f);
}

void describe(FILE* f, Token::Notation n){
  fputs(stringify(n), f);
}

void describe(FILE* f, const Token& tok){
  const auto t = tok.type();

  fprintf(f, "Token: %s(", stringify(t));

  switch(t){
  case Token::Type::uninitialized:
    break;
  case Token::Type::identifier:
  case Token::Type::string:
    fputs(tok.get<string>().c_str(), f);
    break;
  case Token::Type::boolean:
    fputs(tok.get<bool>() ? "true" : "false", f);
    break;
  case Token::Type::number:
    describe(f, tok.get<Number>());
    break;
  case Token::Type::character:
    fputc(tok.get<char>(), f);
    break;
  case Token::Type::notation:
    describe(f, tok.get<Token::Notation>());
    break;
  default:
    UNEXP_DEFAULT();
  }

  fputc(')', f);
}
