#include "symtable.hh"

#include <utility>

#include "symbol.hh"
#include "keyword.hh"

using namespace std;

Symbol* SymTable::intern(const string& s){
  auto ret = table_.insert(make_pair(s, Symbol(s)));
  return &(get<0>(ret)->second);
}

Symbol* SymTable::intern(Keyword k){
  return intern(string{stringify(k)});
}

void SymTable::unintern(Symbol* s){
  table_.erase(s->name());
}
