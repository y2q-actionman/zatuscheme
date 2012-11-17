#include "token.hh"
#include "util.hh"

using namespace std;

inline
Token::Token(const std::string& s, Type t)
  : type_(t), str_(s){}

inline
Token::Token(std::string&& s, Type t)
  : type_(t), str_(std::move(s)){}

inline
Token::Token(const Number& n)
  : type_(Type::number), num_(n){}

inline
Token::Token(Number&& n)
  : type_(Type::number), num_(std::move(n)){}

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
  type_ = other.type_;

  switch(other.type_){
  case Type::uninitialized:
    break;

  case Type::identifier:
  case Type::string:
    new (&this->str_) string(std::move(other.str_));
    break;

  case Type::boolean:
    this->b_ = other.b_;
    break;

  case Type::number:
    new (&this->num_) Number(std::move(other.num_));
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

Token::Token(const Token& other){
  init_from_other<const Token&>(forward<const Token>(other));
}
  
Token::Token(Token&& other){
  init_from_other<Token&&>(forward<Token>(other));
}

template<typename T>
inline
Token& Token::assign_from_other(T other){
  switch(this->type_){
  case Type::identifier:
  case Type::string:
    if(other.type_ == Type::string || other.type_ == Type::identifier){
      this->type_ = other.type_;
      this->str_ = std::move(other.str_);
      return *this;
    }else{
      str_.~string();
      break;
    }

  case Type::number:
    if(other.type_ == Type::number){
      this->num_ = std::move(other.num_);
      return *this;
    }else{
      num_.~Number();
      break;
    }

  case Type::uninitialized:
  case Type::boolean:
  case Type::character:
  case Type::notation:
    break;

  default:
    UNEXP_DEFAULT();
  }

  new (this) Token(std::move(other));
  return *this;
}

Token& Token::operator=(const Token& other){
  return assign_from_other<const Token&>(forward<const Token>(other));
}

Token& Token::operator=(Token&& other){
  return assign_from_other<Token&&>(forward<Token>(other));
}

Token::~Token(){
  switch(type_){
  case Type::identifier:
  case Type::string:
    str_.~string();
    break;

  case Type::number:
    num_.~Number();
    break;

  case Type::uninitialized:
  case Type::boolean:
  case Type::character:
  case Type::notation:
    break;

  default:
    //UNEXP_DEFAULT();
    break;
  }

  type_ = Type::uninitialized;
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
    
  while((c = fgetc(f)) != EOF
        && (isspace(c) || c == ';')){
    if(c == ';'){
      do{
        c = fgetc(f);
      }while(c != EOF && c != '\n');
    }
  }
  ungetc(c, f);
}


Token tokenize_identifier(FILE* f, int first_char){
  string s(1, first_char);

  // subsequent
  decltype(fgetc(f)) c;

  while(!is_delimiter(c = fgetc(f))
        && (isalpha(c) || is_special_initial(c) 
            || isdigit(c) || c == '+' || c == '-'
            || c == '.' || c == '@')){
    s.push_back(c);
  }
  ungetc(c, f);

  assert(!s.empty());
  return Token{s, Token::Type::identifier};
}

Token tokenize_character(FILE* f){
  // function for checking character-name
  const auto check_name = [&](const char* str) -> bool {
    for(const char* c = str; *c; ++c){
      auto get_c = fgetc(f);
      if(get_c != tolower(*c) || is_delimiter(get_c)){
        ungetc(get_c, f);
        return false;
      }
    }
    return true;
  };

  auto ret_char = fgetc(f);
  if(ret_char == EOF){
    throw zs_error("reader error: no char found after '#\\'!\n");
  }

  auto next = fgetc(f);
  ungetc(next, f);

  if(is_delimiter(next)){
    return Token{static_cast<char>(ret_char)};
  }else{
    // check character name
    switch(tolower(ret_char)){
    case 's':
      if(check_name("pace")){
        return Token{' '};
      }
      break;
    case 'n':
      if(check_name("ewline")){
        return Token{'\n'};
      }
      break;
    }

    throw zs_error("reader error: not supprted char name!\n");
  }
}

Token tokenize_string(FILE* f){
  decltype(fgetc(f)) c;
  string s;

  while((c = fgetc(f)) != EOF){
    switch(c){
    case '"':
      return Token{s, Token::Type::string};
    case '\\':
      switch(c = fgetc(f)){
      case '"': case '\\':
        s.push_back(c);
        break;
      default:
        throw make_zs_error("reader error: unknown string escape '%c' appeared.\n", c);
      }
      break;
    default:
      s.push_back(c);
    }
  }

  throw zs_error("reader error: not ended string!\n");
}

Token tokenize_number(FILE* f, int read_c = 0){
  if(read_c){
    if(ungetc(read_c, f) == EOF){
      throw zs_error("reader internal error: fatal I/O error occured. (reached unreading limit)\n");
    }
  }

  if(auto n = parse_number(f)){
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
    decltype(fgetc(f)) c2;

    while((c2 = fgetc(f)) == '.'){
      ++dots;
    }
    ungetc(c2, f);

    switch(dots){
    case 1:
      return Token{Token::Notation::dot};
    case 3:
      return Token{"...", Token::Type::identifier};
    default:
      throw make_zs_error("reader error: %d dots appeared.\n", dots);
    }
  }

  case '"':
    return tokenize_string(f);

  case '+': case '-': {
    auto c2 = fgetc(f);
    ungetc(c2, f);

    if(is_delimiter(c2)){
      return Token{string(1, c), Token::Type::identifier};
    }else{
      return tokenize_number(f, c);
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
      ungetc(sharp_c, f);
      return tokenize_number(f, '#');
    case '<':
      // cleaning input stream
      do{
        c = fgetc(f);
      }while(c != EOF && c != '>');
      ungetc(c, f);

      throw zs_error("reader error: '#<...>' appeared ('not printable object' in this implementation.)\n");
    default:
      throw make_zs_error("reader error: unknown sharp syntax '#%c' appeared.\n", sharp_c);
    }

  case EOF:
    return Token{static_cast<char>(EOF)};

  default:
    if(isalpha(c) || is_special_initial(c)){
      return tokenize_identifier(f, c);
    }else if(isdigit(c)){
      ungetc(c, f);
      return tokenize_number(f);
    }else{
      throw make_zs_error("reader error: invalid char '%c' appeared.\n", c);
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
