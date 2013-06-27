#ifndef TOKEN_HH
#define TOKEN_HH

#include <iosfwd>

#include "decl.hh"

enum class Notation {
  l_paren, r_paren, vector_paren,
    quote, quasiquote, comma, comma_at, dot,
    l_bracket, r_bracket,
    l_brace, r_brace,
    bar
    };

// when reached EOF, returns eof-object.
Lisp_ptr tokenize(std::istream&);

// 'radix == 0' means 10 or the specified value of prefix.
Lisp_ptr parse_number(std::istream&, int radix = 0);

const char* stringify(Notation);

#endif // TOKEN_HH
