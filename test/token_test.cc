#include <string>
#include <sstream>
#include <utility>
#include <cstdlib>
#include <memory>
#include <cstdio>
#include <cstring>

#include "token.hh"
#include "test_util.hh"
#include "describe.hh"

#define PRINT_BUFSIZE 100

using namespace std;

static bool result;

template<typename T>
void check_copy_move(const Token& tok){
  // constructor
  {
    Token tok_c_con(tok);
    if(tok.type() != tok_c_con.type() || tok.get<T>() != tok_c_con.get<T>()){
      fputs("[failed] on copy construct: ", zs::err);
      goto error;
    }else{
      Token tok_m_con{move(tok_c_con)};
      if(tok.type() != tok_m_con.type() || tok.get<T>() != tok_m_con.get<T>()){
        fputs("[failed] on move construct ", zs::err);
        goto error;
      }
    }
  }
  
  // operator=
  {
    Token tok_c_op = tok;
    if(tok.type() != tok_c_op.type() || tok.get<T>() != tok_c_op.get<T>()){
      fputs("[failed] on copy operator= ", zs::err);
      goto error;
    }else{
      Token tok_m_op = move(tok_c_op);
      if(tok.type() != tok_m_op.type() || tok.get<T>() != tok_m_op.get<T>()){
        fputs("[failed] on move operator= ", zs::err);
        goto error;
      }
    }
  }

  return;

 error:
  result = false;
  describe(zs::err, tok);
  fputc('\n', zs::err);
  return;
}

template<typename Fun>
void fail_message(Token::Type t, FILE* f, const fpos_t* b_pos,
                  const Token& tok, const Fun& callback){
  // extract input from stream
  char buf[PRINT_BUFSIZE];

  fsetpos(f, b_pos);
  if(!fgets(buf, sizeof(buf), f)){
    strcpy(buf, "(read error)");
  }

  fprintf(zs::err, "[failed] input='%s', expect type='%s'",
          buf, stringify(t));

  callback();

  fputs("\n\tgotten token: ", zs::err);
  describe(zs::err, tok);
  fputc('\n', zs::err);

  result = false;
}


// for error case
template<>
inline
Token::Type Token::get<Token::Type>() const{
  return type_;
}

void check(FILE* f){
  static const auto type = Token::Type::uninitialized;
  fpos_t init_pos;
  fgetpos(f, &init_pos);

  Token tok;
  { 
    with_null_stream wns;
    tok = tokenize(f);
  }

  if(tok.type() != type){
    fail_message(type, f, &init_pos, tok, [](){});
    return;
  }
  
  check_copy_move<Token::Type>(tok);
}

void check(const string& input){
  Port p{(void*)input.c_str(), input.size()};
  check(p.stream());
}


// for normal cases
template<Token::Type type, typename Fun, 
         typename ex_type = decltype(to_type<type>())>
void check_generic(FILE* f, const ex_type& expect,
                   const Fun& fun){
  fpos_t init_pos;
  fgetpos(f, &init_pos);

  const Token tok = tokenize(f);

  if(tok.type() != type || tok.get<ex_type>() != expect){
    fail_message(type, f, &init_pos, tok, fun);
    return;
  }
  
  check_copy_move<ex_type>(tok);
}

template<Token::Type T>
void check(FILE* f, const string& expect){
  check_generic<T>
    (f, expect,
     [&](){
      fprintf(zs::err, ", expected str='%s'", expect.c_str());
    });
}

template<Token::Type T>
void check(const string& input, const string& expect){
  Port p{(void*)input.c_str(), input.size()};
  check<T>(p.stream(), expect);
}

#define check_ident check<Token::Type::identifier>
#define check_string check<Token::Type::string>

inline
bool operator==(const Number& n1, const Number& n2){
  return eqv(n1, n2);
}

inline
bool operator!=(const Number& n1, const Number& n2){
  return !eqv(n1, n2);
}

void check(FILE* f, const Number& n){
  check_generic<Token::Type::number>
    (f, n, 
     [=](){
      fprintf(zs::err, ", expected num='");
      describe(zs::err, n);
    });
}

void check(FILE* f, bool expect){
  check_generic<Token::Type::boolean>
    (f, expect, 
     [=](){
      fprintf(zs::err, ", expected bool='%s'", expect ? "true" : "false");
    });
}

void check(FILE* f, char expect){
  check_generic<Token::Type::character>
    (f, expect, 
     [=](){
      fprintf(zs::err, ", expected char='%c'", expect);
    });
}

void check(FILE* f, Token::Notation n){
  check_generic<Token::Type::notation>
    (f, n,
     [=](){
      fputs(", expected notation='", zs::err);
      describe(zs::err, n);
      fputc('\'', zs::err);
    });
}

template<typename T>
void check(const string& input, T&& expect){
  Port p{(void*)input.c_str(), input.size()};
  check(p.stream(), expect);
}


#define N Token::Notation

int main(){
  result = true;
  open_null_stream();

  // identifier
  check_ident("lambda", "lambda");
  check_ident("q", "q");
  check_ident("list->vector", "list->vector");
  check_ident("soup", "soup");
  check_ident("+", "+");
  check_ident("V17a", "V17a");
  check_ident("<=?", "<=?");
  check_ident("a34kTMNs", "a34kTMNs");
  check_ident("the-word-resursin-has-many-meanings", "the-word-resursin-has-many-meanings");

  check_ident("+",  "+");
  check_ident("-",  "-");
  check_ident("...",  "...");

  check(".."); // error
  check("...."); // error

  // spaces & comments
  check_ident("   abc", "abc");
  check_ident("   abc    def ", "abc");
  check_ident(" ;hogehoge\n   abc", "abc");
  check(" ;hogehoge\n");
  check_ident("   abc;fhei", "abc");

  // boolean
  check("#t", true);
  check("#f", false);

  // number
  check("+1", Number{1l});
  check("#x16", Number{0x16l});
  
  // character
  check("#\\");
  check("#\\a", 'a');
  check("#\\b", 'b');
  check("#\\x", 'x');
  check("#\\s", 's');
  check("#\\space", ' ');
  check("#\\newline", '\n');

  // string
  check("\"");
  check_string("\"\"", "");
  check_string("\"a\"", "a");
  check_string("\"abcd\nedf jfkdj\"", "abcd\nedf jfkdj");
  check_string("\"\\\"\\\"\"", "\"\"");

  // error
  check("#z");


  // consecutive access
  {
    char teststr[] = "(a . b)#(c 'd) e ...;comment\nf +11 `(,x ,@y \"ho()ge\")";
    Port p{teststr, sizeof(teststr)};
    auto ss = p.stream();

    check(ss, N::l_paren);
    check_ident(ss, "a");
    check(ss, N::dot);
    check_ident(ss, "b");
    check(ss, N::r_paren);
    check(ss, N::vector_paren);
    check_ident(ss, "c");
    check(ss, N::quote);
    check_ident(ss, "d");
    check(ss, N::r_paren);
    check_ident(ss, "e");
    check_ident(ss, "...");

    check_ident(ss, "f");
    check(ss, Number{11l});
    check(ss, N::quasiquote);
    check(ss, N::l_paren);
    check(ss, N::comma);
    check_ident(ss, "x");
    check(ss, N::comma_at);
    check_ident(ss, "y");
    check_string(ss, "ho()ge");
    check(ss, N::r_paren);

    check(ss);
  }

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
