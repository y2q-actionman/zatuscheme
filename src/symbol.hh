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
  { return name_ ; }

  Keyword keyword() const
  { return k_; }

private:
  Symbol() = delete;
  explicit Symbol(const std::string&);
  explicit Symbol(std::string&&);

  Symbol& operator=(const Symbol&) = default;
  Symbol& operator=(Symbol&&) = default;

private:
  std::string name_;
  Keyword k_;
  // add pointer to symbol table?
};

#include "symbol.i.hh"

#endif // SYMBOL_HH
