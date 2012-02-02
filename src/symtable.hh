#ifndef SYMTABLE_HH
#define SYMTABLE_HH

#include <unordered_map>
#include <string>

#include "symbol.hh"

enum class Keyword;

class SymTable {
public:
  SymTable() = default;
  SymTable(const SymTable&) = default;
  SymTable(SymTable&&) = default;

  ~SymTable() = default;

  SymTable& operator=(const SymTable&) = default;
  SymTable& operator=(SymTable&&) = default;

  template<typename T>
  Symbol* intern(T);

  void unintern(Symbol*);

private:
  std::unordered_map<std::string, Symbol> table_;
};

#endif //SYMTABLE_HH
