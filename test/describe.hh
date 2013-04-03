#ifndef DESCRIBE_HH
#define DESCRIBE_HH

#include "zs.hh"
#include <iosfwd>

std::ostream& operator<<(std::ostream&, Ptr_tag);
// std::ostream& operator<<(std::ostream&, Lisp_ptr);

std::ostream& operator<<(std::ostream&, const ProcInfo&);
std::ostream& operator<<(std::ostream&, ProcFlag::Variadic);

std::ostream& operator<<(std::ostream&, Token::Type);
std::ostream& operator<<(std::ostream&, Token::Notation);
std::ostream& operator<<(std::ostream&, const Token&);

#endif //DESCRIBE_HH
