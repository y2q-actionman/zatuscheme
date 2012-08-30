#ifndef DESCRIBE_HH
#define DESCRIBE_HH

#include "zs.hh"

void describe(FILE*, Lisp_ptr);

void describe(FILE*, Number::Type);
void describe(FILE*, const Number&);

void describe(FILE*, const Procedure::ArgInfo&);

void describe(FILE*, Token::Type);
void describe(FILE*, Token::Notation);
void describe(FILE*, const Token&);

#endif //DESCRIBE_HH
