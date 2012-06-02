#ifndef SYMBOL_HH
#define SYMBOL_HH

#include <string>
#include "keyword.hh"

class Symbol{
public:
  friend class SymTable;

  Symbol(const Symbol&) = default;
  Symbol(Symbol&&) = default;

  ~Symbol() = default;

  const std::string& name() const
  { return *name_ ; }

  Keyword to_keyword() const
  { return k_; }

private:
  constexpr Symbol() : name_(nullptr), k_(Keyword::not_keyword){}

  Symbol& operator=(const Symbol&) = delete;
  Symbol& operator=(Symbol&&) = delete;

  void rebind(const std::string*);

private:
  const std::string* name_;
  Keyword k_;
  // add pointer to symbol table?
};

#include "symbol.i.hh"

#endif // SYMBOL_HH
