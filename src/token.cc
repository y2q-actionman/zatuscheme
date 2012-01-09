#include <cctype>
#include <cstring>
#include <istream>
#include <sstream>
#include <stdexcept>
#include <limits>
#include "token.hh"

using namespace std;

template<typename charT>
static inline constexpr
bool is_delimiter(charT c){
  return isspace(c) || c == '(' || c == ')'
    || c ==  '"' || c == ';';
}

template<typename charT>
static inline
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

static
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


bool Token::tokenize_identifier(istream& i){
  auto c = i.peek();

  // initial
  if(isalpha(c) || is_special_initial(c)){
    ostringstream s;

    // subsequent
    do{
      s.put(static_cast<char>(i.get()));
      c = i.peek();
    }while(isalpha(c) || is_special_initial(c) 
           || isdigit(c) || c == '+' || c == '-'
           || c == '.' || c == '@');

    type_ = Token::Type::identifier;
    str_ = s.str();
    return true;
  }else{
    // peculiar_identifier
    if(c == '+' || c ==  '-'){
      type_ = Token::Type::identifier;
      str_ = string(1, c);
      return true;
    }else if(c == '.'){
      const auto init_pos = i.tellg();
      int dots = 0;

      do{
        i.ignore(1);
        ++dots;
      }while(dots < 3 && i.peek() != '.');

      if(dots != 3){
        i.seekg(init_pos);
        goto error;
      }

      type_ = Token::Type::identifier;
      str_ = string(3, '.');
      return true;
    }
  }

 error:
  return false;
}

bool Token::tokenize_boolean(istream& i){
  if(i.peek() == '#'){
    ostringstream s;

    s.put(static_cast<char>(i.get()));

    switch(i.peek()){
    case 't': case 'f':
      s.put(static_cast<char>(i.get()));

      type_ = Token::Type::boolean;
      str_ = s.str();
      return true;
    default:
      i.unget();
      goto error;
    }
  }

 error:
  return false;
}

bool Token::tokenize_character(istream& i){
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
  s.put(static_cast<char>(i.get()));

  if(i.peek() != '\\') goto error;
  s.put(static_cast<char>(i.get()));

  type_ = Token::Type::character;

  switch(i.peek()){
  case 's': {
    static const char target[] = "space";
    if(check_and_push(target, sizeof(target))){
      str_ = s.str();
      break;
    }else{
      goto single_char;
    }
  }
  case 'n': {
    static const char target[] = "newline";
    if(check_and_push(target, sizeof(target))){
      str_ = s.str();
      break;
    }else{
      goto single_char;
    }
  }
  default: 
  single_char:
    s.put(static_cast<char>(i.get()));
    str_ = s.str();
  }

  return true;

 error:
  i.seekg(pos);
  return false;
}

bool Token::tokenize_string(istream& i){
  auto pos = i.tellg();
  ostringstream s;

  if(i.peek() == '"') goto error;

  do{
    s.put(static_cast<char>(i.get()));

    switch(i.peek()){
    case '"':
      s.put(static_cast<char>(i.get()));
      type_ = Token::Type::string;
      str_ = s.str();
      return true;
    case '\\':
      s.put(static_cast<char>(i.get()));

      switch(i.peek()){
      case '"': case '\\':
        s.put(static_cast<char>(i.get()));
        break;
      default:
        goto error;
      }
      break;
    }
  }while(i);

 error:
  i.seekg(pos);
  return false;
}


Token::Token(std::istream& o)
  : type_(Type::uninitialized), str_()
{
  if(tokenize_identifier(o)
     || tokenize_boolean(o)
     || tokenize_character(o)
     || tokenize_string(o)){
    return;
  }

  throw runtime_error("cannot be tokenized");
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
