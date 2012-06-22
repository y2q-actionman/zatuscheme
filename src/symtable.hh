#ifndef SYMTABLE_HH
#define SYMTABLE_HH

#include <unordered_map>
#include <string>

#include "symbol.hh"

class SymTable {
public:
  SymTable() = default;
  SymTable(const SymTable&) = default;
  SymTable(SymTable&&) = default;

  ~SymTable() = default;

  SymTable& operator=(const SymTable&) = default;
  SymTable& operator=(SymTable&&) = default;

  Symbol* intern(const std::string&);
  void unintern(Symbol*);

private:
  std::unordered_map<std::string, Symbol> table_;
};

#endif //SYMTABLE_HH
