#ifndef SYMBOL_HH
#define SYMBOL_HH

#include <string>
#include "symtable.hh"

class Symbol{
public:
  friend Symbol* intern(SymTable&, const std::string&);

  Symbol(const Symbol&) = default;
  Symbol(Symbol&&) = default;

  ~Symbol() = default;

  const std::string& name() const
  { return *name_ ; }

private:
  constexpr Symbol() : name_(nullptr){}

  Symbol& operator=(const Symbol&) = delete;
  Symbol& operator=(Symbol&&) = delete;

  void rebind(const std::string*);

private:
  const std::string* name_;
  // add pointer to symbol table?
};

#include "symbol.i.hh"

#endif // SYMBOL_HH
