#include "symtable.hh"

#include <utility>

#include "symbol.hh"
#include "keyword.hh"

using namespace std;

template<typename T>
Symbol* SymTable::intern(T s){
  auto ret = table_.find(s);

  if(ret != table_.end()){
    return &(ret->second);
  }else{
    // use 'emplace' when implemented.
    auto i_ret = table_.insert(make_pair(s, Symbol(s)));
    return &(get<0>(i_ret)->second);
  }
}

template Symbol* SymTable::intern(const string&);
template Symbol* SymTable::intern(string&&);

template<>
Symbol* SymTable::intern(Keyword k){
  return intern(stringify(k));
}

void SymTable::unintern(Symbol* s){
  table_.erase(s->name());
}
