#include <cctype>
#include <cstring>
#include <istream>
#include <sstream>
#include <stdexcept>
#include <limits>
#include <cstdio>
#include <cstdlib>

#include "token.hh"

using namespace std;

static
void
__attribute__((noreturn))// [[noreturn]]
unexp_default(const char* f, int l){
  fprintf(stderr, "unexpected default case! (file=%s, line=%d)", f, l);
  abort();
}

#define UNEXP_DEFAULT() unexp_default(__FILE__, __LINE__)


Token::Token(const Token& other)
  : type_(other.type_)
{
  switch(other.type_){
  case Type::uninitialized:
    break;

  case Type::identifier:
  case Type::string:
    new (&this->str_) string(other.str_);
    break;

  case Type::boolean:
    this->b_ = other.b_;
    break;

  case Type::number:
    new (&this->num_) Number(other.num_);
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
  
Token::Token(Token&& other)
  : type_(other.type_)
{
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

Token& Token::operator=(const Token& other){
  switch(this->type_){
  case Type::uninitialized:
    new (this) Token(other);
    break;
    
  case Type::identifier:
  case Type::string:
    switch(other.type()){
    case Type::identifier:
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

  case Type::character:
    if(other.type() == this->type()){
      this->c_ = other.c_;
    }else{
      new (this) Token(other);
    }
    break;

  case Type::notation:
    if(other.type() == this->type()){
      this->not_ = other.not_;
    }else{
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
  case Type::string:
    switch(other.type()){
    case Type::identifier:
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

streamoff skip_intertoken_space(istream& i, streamoff skipped = 0){
  const auto c = i.peek();

  if(isspace(c)){
    return skip_intertoken_space(i.ignore(1), skipped + 1);
  }else if(c == ';'){
    const auto pos = i.tellg();

    do{
      char tmp[100];
      i.getline(tmp, sizeof(tmp));
    }while(!i.eof() && i.fail());
    
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
    }while(!is_delimiter(c) 
           || isalpha(c) || is_special_initial(c) 
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

      while(i.get() == '.' && dots <= 3){
        ++dots;
      }

      if(dots != 3){
        i.clear();
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
  const auto pos = i.tellg();

  if(i.peek() == '#'){
    i.get();

    switch(i.peek()){
    case 't':
      return Token{true};
    case 'f':
      return Token{false};
    default:
      i.clear();
      i.seekg(pos);
      goto error;
    }
  }

 error:
  return Token{};
}

Token tokenize_character(istream& i){
  const auto pos = i.tellg();
  char ret_char;

  // function for checking character-name
  const auto check_name = [&](const char* str) -> bool {
    const auto initpos = i.tellg();

    for(const char* c = str; *c; ++c){
      auto get_c = i.get();
      if(get_c != *c || is_delimiter(get_c)){
        i.clear();
        i.seekg(initpos);
        return false;
      }
    }
    return true;
  };

  if(i.peek() != '#') goto error;
  i.ignore(1);

  if(i.peek() != '\\') goto error;
  i.ignore(1);

  // check character name
  switch(i.peek()){
  case 's': {
    if(check_name("space")){
      ret_char = ' ';
      break;
    }else{
      goto single_char;
    }
  }
  case 'n': {
    if(check_name("newline")){
      ret_char = '\n';
      break;
    }else{
      goto single_char;
    }
  }
  case EOF:
    goto error;
  default: 
  single_char:
    ret_char = i.get();
    break;
  }

  return Token{static_cast<char>(ret_char)};

 error:
  i.clear();
  i.seekg(pos);
  return Token{};
}

Token tokenize_string(istream& i){
  const auto pos = i.tellg();
  ostringstream s;

  if(i.peek() != '"') goto error;
  i.ignore(1);

  while(i){
    switch(i.peek()){
    case '"':
      i.ignore(1);
      return Token{s.str(), Token::Type::string};
    case '\\':
      i.ignore(1);
      switch(i.peek()){
      case '"': case '\\':
        s.put(i.get());
        break;
      default:
        goto error;
      }
      break;
    default:
      s.put(i.get());
    }
  }

 error:
  i.clear();
  i.seekg(pos);
  return Token{};
}

Token tokenize_notation(istream& i){
  const auto pos = i.tellg();

  switch(i.get()){
  case '(':
    return Token{Token::Notation::l_paren};
  case ')':
    return Token{Token::Notation::r_paren};
  case '#':
    if(i.peek() == '('){
      i.ignore(1);
      return Token{Token::Notation::vector_paren};
    }else{
      i.clear();
      i.seekg(pos);
      return Token{};
    }
  case '\'':
    return Token{Token::Notation::quote};
  case '`':
    return Token{Token::Notation::quasiquote};
  case ',':
    if(i.peek() == '@'){
      i.ignore(1);
      return Token{Token::Notation::comma_at};
    }else{
      return Token{Token::Notation::comma};
    }
  case '.':
    {
      const auto c = i.peek();
      if(c == '.'){
        i.clear();
        i.seekg(pos);
        return Token{};
      }else{
        return Token{Token::Notation::dot};
      }
    }
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
    i.clear();
    i.seekg(pos);
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

  skip_intertoken_space(i);

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

  if(is_inited(t = tokenize_notation(i)))
    return t;
 
  return Token{};
}

namespace {

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
    UNEXP_DEFAULT();
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
    UNEXP_DEFAULT();
  }    
}

} // namespace

void describe(ostream& o, Token::Type t){
  o << stringify(t);
}

void describe(ostream& o, Token::Notation n){
  o << stringify(n);
}

void describe(ostream& o, const Token& tok){
  const auto t = tok.type();

  o << "Token: " << stringify(t);

  o << "(";
  switch(t){
  case Token::Type::uninitialized:
    break;
  case Token::Type::identifier:
  case Token::Type::string:
    o << tok.get<string>();
    break;
  case Token::Type::boolean:
    o << tok.get<bool>();
    break;
  case Token::Type::number:
    describe(o, tok.get<Number>());
    break;
  case Token::Type::character:
    o << tok.get<char>();
    break;
  case Token::Type::notation:
    describe(o, tok.get<Token::Notation>());
    break;
  default:
    UNEXP_DEFAULT();
  }    
  o << ")";
}
