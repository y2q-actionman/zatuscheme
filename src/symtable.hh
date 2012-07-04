#ifndef SYMTABLE_HH
#define SYMTABLE_HH

#include <unordered_map>
#include <string>

class Symbol;

typedef std::unordered_map<std::string, Symbol> SymTable;

Symbol* intern(SymTable&, const std::string&);
void unintern(SymTable&, Symbol*);

#endif //SYMTABLE_HH
