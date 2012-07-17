#include <string>
#include <sstream>
#include <utility>
#include <cstdlib>
#include <memory>
#include <cstdio>
#include <cstring>

#include "token.hh"

#define PRINT_BUFSIZE 100

using namespace std;

static bool result;

// for debugging..
template<>
inline
Token::Type Token::get<Token::Type>() const{
  return type_;
}

template<typename T>
void check_copy_move(const Token& tok){
  // constructor
  {
    Token tok_c_con(tok);
    if(tok.type() != tok_c_con.type() || tok.get<T>() != tok_c_con.get<T>()){
      fputs("[failed] on copy construct: ", stdout);
      goto error;
    }else{
      Token tok_m_con{move(tok_c_con)};
      if(tok.type() != tok_m_con.type() || tok.get<T>() != tok_m_con.get<T>()){
        fputs("[failed] on move construct ", stdout);
        goto error;
      }
    }
  }
  
  // operator=
  {
    Token tok_c_op = tok;
    if(tok.type() != tok_c_op.type() || tok.get<T>() != tok_c_op.get<T>()){
      fputs("[failed] on copy operator= ", stdout);
      goto error;
    }else{
      Token tok_m_op = move(tok_c_op);
      if(tok.type() != tok_m_op.type() || tok.get<T>() != tok_m_op.get<T>()){
        fputs("[failed] on move operator= ", stdout);
        goto error;
      }
    }
  }

  return;

 error:
  result = false;
  describe(stdout, tok);
  fputc('\n', stdout);
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

  fprintf(stdout, "[failed] input='%s', expect type='%s'",
          buf, stringify(t));

  callback();

  fputs("\n\tgotten token: ", stdout);
  describe(stdout, tok);
  fputc('\n', stdout);

  result = false;
}

template<Token::Type type, typename Fun>
void check_generic(FILE* f, const Fun& fun){
  fpos_t init_pos;
  fgetpos(f, &init_pos);

  const Token tok = tokenize(f);

  if(tok.type() != type){
    fail_message(type, f, &init_pos, tok, fun);
    return;
  }
  
  check_copy_move<Token::Type>(tok);
}

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


void check(FILE* f){
  check_generic<Token::Type::uninitialized>
    (f, [](){});
}

void check(const string& input){
  FILE* f = fmemopen((void*)input.c_str(), input.size(), "r");
  check(f);
  fclose(f);
}

template<Token::Type T>
void check(FILE* f, const string& expect){
  check_generic<T>
    (f, expect,
     [&](){
      fprintf(stdout, ", expected str='%s'", expect.c_str());
    });
}

template<Token::Type T>
void check(const string& input, const string& expect){
  FILE* f = fmemopen((void*)input.c_str(), input.size(), "r");
  check<T>(f, expect);
  fclose(f);
}

inline
bool operator==(const Number& n1, const Number& n2){
  return eql(n1, n2);
}

inline
bool operator!=(const Number& n1, const Number& n2){
  return !eql(n1, n2);
}

void check(FILE* f, const Number& n){
  check_generic<Token::Type::number>
    (f, n, 
     [=](){
      fprintf(stdout, ", expected num='");
      describe(stdout, n);
    });
}

void check(FILE* f, bool expect){
  check_generic<Token::Type::boolean>
    (f, expect, 
     [=](){
      fprintf(stdout, ", expected bool='%s'", expect ? "true" : "false");
    });
}

void check(FILE* f, char expect){
  check_generic<Token::Type::character>
    (f, expect, 
     [=](){
      fprintf(stdout, ", expected char='%c'", expect);
    });
}

void check(FILE* f, Token::Notation n){
  check_generic<Token::Type::notation>
    (f, n,
     [=](){
      fputs(", expected notation='", stdout);
      describe(stdout, n);
      fputc('\'', stdout);
    });
}

template<typename T>
void check(const string& input, T&& expect){
  FILE* f = fmemopen((void*)input.c_str(), input.size(), "r");
  check(f, expect);
  fclose(f);
}


#define check_ident check<Token::Type::identifier>
#define check_string check<Token::Type::string>
#define N Token::Notation

int main(){
  result = true;

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
    FILE* ss = fmemopen(teststr, sizeof(teststr), "r");

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

    fclose(ss);
  }

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

