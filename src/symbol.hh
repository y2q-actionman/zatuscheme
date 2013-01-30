#ifndef SYMBOL_HH
#define SYMBOL_HH

#include <unordered_map>
#include <string>
#include <iosfwd>

class Symbol;

// SymTable declarations.
typedef std::unordered_map<std::string, Symbol> SymTable;

std::ostream& operator<<(std::ostream&, const SymTable&);

// Symbol declarations.
class Symbol{
public:
  // makes uninterned symbol (gensym)
  constexpr Symbol(const std::string* s) : name_(s){}

  Symbol(const Symbol&) = default;
  Symbol(Symbol&&) = default;

  ~Symbol() = default;

  const std::string& name() const
  { return *name_ ; }

  friend Symbol* intern(SymTable&, const std::string&);
  friend void unintern(SymTable&, Symbol*);

private:
  constexpr Symbol();

  Symbol& operator=(const Symbol&) = delete;
  Symbol& operator=(Symbol&&) = delete;

  void rebind(const std::string*);

private:
  const std::string* name_;
  // add pointer to symbol table?
};

#endif // SYMBOL_HH
