#ifndef SYMBOL_HH
#define SYMBOL_HH

#include <unordered_map>
#include <string>

class Symbol;

// SymTable declarations.
typedef std::unordered_map<std::string, Symbol> SymTable;

// Symbol declarations.
class Symbol{
public:
  friend Symbol* intern(SymTable&, const std::string&);
  friend void unintern(SymTable&, Symbol*);

  Symbol(const Symbol&) = default;
  Symbol(Symbol&&) = default;

  ~Symbol() = default;

  const std::string& name() const
  { return *name_ ; }

private:
  constexpr Symbol();

  Symbol& operator=(const Symbol&) = delete;
  Symbol& operator=(Symbol&&) = delete;

  inline void rebind(const std::string*);

private:
  const std::string* name_;
  // add pointer to symbol table?
};

#include "symbol.i.hh"

#endif // SYMBOL_HH
