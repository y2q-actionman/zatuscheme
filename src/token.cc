#include <cctype>
#include <cstring>
#include <istream>
#include <sstream>
#include <stdexcept>
#include <limits>
#include <cstdio>
#include <cstdlib>

#include "token.hh"

#define UNEXP_DEFAULT() do{                                             \
  fprintf(stderr, "unexpected default case! (file=%s, line=%d)",        \
          __FILE__, __LINE__);                                          \
  abort();                                                              \
  }while(0)
  

using namespace std;

Token::Token(const Token& other)
  : type_(other.type_)
{
  switch(other.type_){
  case Type::uninitialized:
    break;

  case Type::identifier:
  case Type::character:
  case Type::string:
    new (&this->str_) string(other.str_);
    break;

  case Type::boolean:
    this->b_ = other.b_;
    break;

  case Type::number:
    new (&this->num_) Number(other.num_);
    break;

  case Type::notation:
    new (&this->not_) Notation(other.not_);
    break;

  default:
    UNEXP_DEFAULT();
  }
}
  
Token::Token(Token&& other)
  : type_(other.type_)
{
  switch(other.type_){
  case Type::uninitialized:
    break;

  case Type::identifier:
  case Type::character:
  case Type::string:
    new (&this->str_) string(move(other.str_));
    break;

  case Type::boolean:
    this->b_ = other.b_;
    break;

  case Type::number:
    new (&this->num_) Number(move(other.num_));
    break;

  case Type::notation:
    new (&this->not_) Notation(move(other.not_));
    break;

  default:
    UNEXP_DEFAULT();
  }
}

Token& Token::operator=(const Token& other){
  switch(this->type_){
  case Type::uninitialized:
    new (this) Token(other);
    break;
    
  case Type::identifier:
  case Type::character:
  case Type::string:
    switch(other.type()){
    case Type::identifier:
    case Type::character:
    case Type::string:
      this->str_ = other.str_;
      break;
    default:
      str_.~string();
      new (this) Token(other);
    }
    break;

  case Type::boolean:
    if(other.type() == this->type()){
      b_ = other.b_;
    }else{
      new (this) Token(other);
    }
    break;

  case Type::number:
    if(other.type() == this->type()){
      this->num_ = other.num_;
    }else{
      num_.~Number();
      new (this) Token(other);
    }
    break;

  case Type::notation:
    if(other.type() == this->type()){
      this->not_ = other.not_;
    }else{
      not_.~Notation();
      new (this) Token(other);
    }
    break;

  default:
    UNEXP_DEFAULT();
  }

  return *this;
}

Token& Token::operator=(Token&& other){
  switch(this->type_){
  case Type::uninitialized:
    new (this) Token(move(other));
    break;
    
  case Type::identifier:
  case Type::character:
  case Type::string:
    switch(other.type()){
    case Type::identifier:
    case Type::character:
    case Type::string:
      this->str_ = move(other.str_);
      break;
    default:
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

  case Type::notation:
    if(other.type() == this->type()){
      this->not_ = move(other.not_);
    }else{
      not_.~Notation();
      new (this) Token(move(other));
    }
    break;

  default:
    UNEXP_DEFAULT();
  }

  return *this;
}

//
// tokenizer funcs
//

namespace {

template<typename charT>
inline constexpr
bool is_delimiter(charT c){
  return isspace(c) || c == '(' || c == ')'
    || c ==  '"' || c == ';';
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

streamoff skip_intertoken_space(istream& i, streamoff skipped = 0){
  auto c = i.peek();

  if(isspace(c)){
    return skip_intertoken_space(i.ignore(1), skipped + 1);
  }else if(c == ';'){
    auto pos = i.tellg();
    i.ignore(numeric_limits<decltype(pos)>::max(), '\n');
    return skip_intertoken_space(i, i.tellg() - pos);
  }

  return skipped;
}


Token tokenize_identifier(istream& i){
  auto c = i.peek();

  // initial
  if(isalpha(c) || is_special_initial(c)){
    ostringstream s;

    // subsequent
    do{
      s.put(i.get());
      c = i.peek();
    }while(isalpha(c) || is_special_initial(c) 
           || isdigit(c) || c == '+' || c == '-'
           || c == '.' || c == '@');

    return Token{s.str(), Token::Type::identifier};
  }else{
    // peculiar_identifier
    if(c == '+' || c ==  '-'){
      return Token{string(1, c), Token::Type::identifier};
    }else if(c == '.'){
      const auto init_pos = i.tellg();
      int dots = 0;

      do{
        i.ignore(1);
        ++dots;
      }while(dots < 3 && i.peek() == '.');

      if(dots != 3){
        i.seekg(init_pos);
        goto error;
      }

      return Token{string(3, '.'), Token::Type::identifier};
    }
  }

 error:
  return Token{};
}

Token tokenize_boolean(istream& i){
  if(i.peek() == '#'){
    i.get();

    switch(i.peek()){
    case 't':
      return Token{true};
    case 'f':
      return Token{false};
    default:
      i.unget();
      goto error;
    }
  }

 error:
  return Token{};
}

Token tokenize_character(istream& i){
  auto pos = i.tellg();
  ostringstream s;

  // function for checking character-name
  auto check_and_push = [&](const char* str, size_t size) -> bool {
    auto charpos = i.tellg();
    char tmp[size];
    i.get(tmp, sizeof(tmp));

    if(strcmp(tmp, str) == 0){
      s << tmp;
      return true;
    }else{
      i.seekg(charpos);
      return false;
    }
  };


  if(i.peek() != '#') goto error;
  s.put(i.get());

  if(i.peek() != '\\') goto error;
  s.put(i.get());

  switch(i.peek()){
  case 's': {
    static const char target[] = "space";
    if(check_and_push(target, sizeof(target))){
      break;
    }else{
      goto single_char;
    }
  }
  case 'n': {
    static const char target[] = "newline";
    if(check_and_push(target, sizeof(target))){
      break;
    }else{
      goto single_char;
    }
  }
  default: 
  single_char:
    s.put(i.get());
    break;
  }

  return Token{s.str(), Token::Type::character};

 error:
  i.seekg(pos);
  return Token{};
}

Token tokenize_string(istream& i){
  auto pos = i.tellg();
  ostringstream s;

  if(i.peek() == '"') goto error;

  do{
    s.put(i.get());

    switch(i.peek()){
    case '"':
      s.put(i.get());
      return Token{s.str(), Token::Type::string};
    case '\\':
      s.put(i.get());

      switch(i.peek()){
      case '"': case '\\':
        s.put(i.get());
        break;
      default:
        goto error;
      }
      break;
    }
  }while(i);

 error:
  i.seekg(pos);
  return Token{};
}

Token tokenize_reserved(istream& i){
  switch(i.peek()){
  case '(':
    return Token{Token::Notation::l_paren};
  case ')':
    return Token{Token::Notation::r_paren};
  case '#':
    i.get();
    if(i.peek() == '('){
      return Token{Token::Notation::vector_paren};
    }else{
      i.unget();
      return Token{};
    }
  case '\'':
    return Token{Token::Notation::quote};
  case '`':
    return Token{Token::Notation::backquote};
  case ',':
    i.get();
    if(i.peek() == '@'){
      return Token{Token::Notation::comma_at};
    }else{
      i.unget();
      return Token{Token::Notation::comma};
    }
  case '.':
    return Token{Token::Notation::dot};
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

  default:
    return Token{};
  }
}

Token tokenize_number(istream& i){
  auto n = parse_number(i);
  if(n.type() != Number::Type::uninitialized){
    return Token{n};
  }else{
    return Token{};
  }
}

inline
bool is_inited(const Token& t){
  return t.type() != Token::Type::uninitialized;
}

} // namespace

Token tokenize(istream& i){
  Token t;

  if(is_inited(t = tokenize_identifier(i)))
    return t;

  if(is_inited(t = tokenize_boolean(i)))
    return t;

  if(is_inited(t = tokenize_number(i)))
    return t;

  if(is_inited(t = tokenize_character(i)))
    return t;

  if(is_inited(t = tokenize_string(i)))
    return t;

  if(is_inited(t = tokenize_reserved(i)))
    return t;
 
  return Token{};
}

bool Token::is_syntactic_keyword() const{
  return is_expression_keyword()
    || str_ == "else" || str_ == "=>"
    || str_ == "define" || str_ == "unquote"
    || str_ == "unquote-splicing";
}

bool Token::is_expression_keyword() const{
  return str_ == "quote" || str_ == "lambda"
    || str_ == "if" || str_ == "set!"
    || str_ == "begin" || str_ == "cond"
    || str_ == "and" || str_ == "or"
    || str_ == "case" || str_ == "let"
    || str_ == "let*" || str_ == "letrec"
    || str_ == "do" || str_ == "delay"
    || str_ == "quasiquote";
}
