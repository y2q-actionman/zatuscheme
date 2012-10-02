#include "describe.hh"
#include "number.hh"

using namespace std;

void describe(FILE* f, Lisp_ptr p){
  fprintf(f, "[%s] %p", stringify(p.tag()), p.get<void*>());
}

void describe(FILE* f, Number::Type t){
  fputs(stringify(t), f);
}

void describe(FILE* f, const Number& n){
  const auto t = n.type();

  fprintf(f, "Number: %s(", stringify(t));
  print(f, n);
  fputc(')', f);
}

void describe(FILE* f, const Procedure::ArgInfo& argi){
  fprintf(f, "[required_args=%d, variadic=%d]",
          argi.required_args, argi.variadic);
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
