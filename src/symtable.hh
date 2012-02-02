#ifndef SYMTABLE_HH
#define SYMTABLE_HH

#include <unordered_set>
#include <string>

class Symbol;

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
  std::unordered_set<std::string> table_;
};

#endif //SYMTABLE_HH
